{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Stackctl.RemovedStackSpec
  ( spec
  ) where

import Stackctl.Test.App

import qualified Amazonka
import qualified Amazonka.CloudFormation as CloudFormation
import Amazonka.CloudFormation.DescribeStacks
import Amazonka.CloudFormation.Types.Stack
import qualified Data.Text as T
import Data.Time (UTCTime (..))
import Data.Time.Calendar (DayOfMonth, MonthOfYear, Year, fromGregorian)
import Network.HTTP.Types.Status (status400)
import Stackctl.AWS.CloudFormation
import Stackctl.DirectoryOption (DirectoryOption (..), directoryOptionL)
import Stackctl.FilterOption (filterOptionFromText, filterOptionL)
import Stackctl.RemovedStack
import UnliftIO.Directory (createDirectoryIfMissing)

spec :: Spec
spec = do
  describe "inferRemovedStacks" $ do
    it "returns stacks in filters that aren't on disk" $ example $ runTestAppT $ do
      let
        Just filterOption =
          filterOptionFromText
            $ T.intercalate
              ","
              [ pack $ testAppStackFilePath "stack-exists"
              , pack $ testAppStackFilePath "stack-is-missing"
              , "stacks/0123456789.test/us-east-2/wrong-region.yaml"
              , "stacks/2123456789.test/us-east-1/wrong-account.yaml"
              ]

        setup :: TestApp -> TestApp
        setup = filterOptionL .~ filterOption

        matchers =
          [ describeStackMatcher "stack-exists" $ Just $ someStack "stack-exists"
          , describeStackMatcher "stack-is-missing" Nothing
          , describeStackMatcher "wrong-region" Nothing
          , describeStackMatcher "wrong-account" Nothing
          ]

      stacks <- local setup $ withMatchers matchers inferRemovedStacks
      map (^. stack_stackName) stacks `shouldBe` ["stack-exists"]

    -- If we don't check for file existence respecting STACKCTL_DIRECTORY, then
    -- any non-default value will cause all specs to appear non-existent and be
    -- flagged for removal. Eek.
    it "respects STACKCTL_DIRECTORY" $ example $ runTestAppT $ do
      let
        dir = "/tmp/stackctl-test"
        toRemove = "stack-to-remove"
        toKeep = "stack-to-keep"
        relativeToRemove = testAppStackFilePath toRemove
        relativeToKeep = testAppStackFilePath toKeep
        absoluteToKeep = dir </> relativeToKeep
        Just filterOption =
          filterOptionFromText
            $ pack relativeToRemove
            <> ","
            <> pack relativeToKeep

        setup :: TestApp -> TestApp
        setup app =
          app
            & filterOptionL .~ filterOption
            & directoryOptionL .~ DirectoryOption dir

        matchers =
          [ describeStackMatcher toRemove $ Just $ someStack toRemove
          , describeStackMatcher toKeep $ Just $ someStack toKeep
          ]

      -- Create a spec on disk for toKeep, then we should only find toRemove
      createDirectoryIfMissing True $ takeDirectory absoluteToKeep
      writeFileUtf8 absoluteToKeep "{}"

      stacks <- local setup $ withMatchers matchers inferRemovedStacks
      map (^. stack_stackName) stacks `shouldBe` [toRemove]

describeStackMatcher :: Text -> Maybe Stack -> Matcher
describeStackMatcher name =
  SendMatcher ((== Just name) . (^. describeStacks_stackName))
    . maybe
      (Left cloudFormationValidationError)
      ( \stack ->
          Right
            $ newDescribeStacksResponse 200
            & describeStacksResponse_stacks ?~ [stack]
      )

someStack :: Text -> Stack
someStack name = newStack name (midnight 2024 1 1) StackStatus_CREATE_COMPLETE

midnight :: Year -> MonthOfYear -> DayOfMonth -> UTCTime
midnight y m d =
  UTCTime
    { utctDay = fromGregorian y m d
    , utctDayTime = 0
    }

cloudFormationValidationError :: Amazonka.Error
cloudFormationValidationError =
  Amazonka.ServiceError
    $ Amazonka.ServiceError'
      { Amazonka.abbrev = CloudFormation.defaultService ^. Amazonka.service_abbrev
      , Amazonka.status = status400
      , Amazonka.headers = []
      , Amazonka.code = "ValidationError"
      , Amazonka.message = Nothing
      , Amazonka.requestId = Nothing
      }
