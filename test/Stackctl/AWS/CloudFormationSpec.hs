module Stackctl.AWS.CloudFormationSpec
  ( spec
  ) where

import Stackctl.Test.App

import Amazonka.CloudFormation.DeleteChangeSet
import Amazonka.CloudFormation.ListChangeSets
import Amazonka.CloudFormation.Types.ChangeSetSummary
import Blammo.Logging.Logger (LoggedMessage (..), getLoggedMessagesUnsafe)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (isSuffixOf)
import Stackctl.AWS.CloudFormation

spec :: Spec
spec = do
  describe "readParameter" $ do
    it "refuses empty key" $ example $ do
      readParameter "=Value"
        `shouldSatisfy` either ("empty KEY" `isSuffixOf`) (const False)

    it "refuses empty value" $ example $ do
      readParameter "Key"
        `shouldSatisfy` either ("empty VALUE" `isSuffixOf`) (const False)

    it "refuses empty value (with =)" $ example $ do
      readParameter "Key="
        `shouldSatisfy` either ("empty VALUE" `isSuffixOf`) (const False)

    it "creates a parameter when valid" $ example $ do
      readParameter "Key=Value=More"
        `shouldBe` Right (makeParameter "Key" $ Just "Value=More")

  describe "awsCloudFormationDeleteAllChangeSets" $ do
    it "deletes all listed changesets" $ example $ runTestAppT $ do
      let
        stackName :: Text
        stackName = "some-stack"

        cs1 :: Text
        cs1 = "some-changeset-1"

        cs2 :: Text
        cs2 = "some-changeset-2"

        cs3 :: Text
        cs3 = "some-changeset-3"

        isListChangeSetsPage :: Maybe Text -> ListChangeSets -> Bool
        isListChangeSetsPage p req =
          and
            [ req ^. listChangeSets_stackName == stackName
            , req ^. listChangeSets_nextToken == p
            ]

        isDeleteChangeSet :: Text -> DeleteChangeSet -> Bool
        isDeleteChangeSet cs req = req ^. deleteChangeSet_changeSetName == cs

        summary1 = newChangeSetSummary & changeSetSummary_changeSetId ?~ cs1
        summary2 = newChangeSetSummary & changeSetSummary_changeSetId ?~ cs2
        summary3 = newChangeSetSummary & changeSetSummary_changeSetId ?~ cs3

        matchers =
          [ SendMatcher (isListChangeSetsPage Nothing)
              $ Right
              $ newListChangeSetsResponse 200
              & (listChangeSetsResponse_summaries ?~ [summary1, summary2])
              & (listChangeSetsResponse_nextToken ?~ "p2")
          , SendMatcher (isListChangeSetsPage $ Just "p2")
              $ Right
              $ newListChangeSetsResponse 200
              & (listChangeSetsResponse_summaries ?~ [summary3])
          , SendMatcher (isDeleteChangeSet cs1)
              $ Right
              $ newDeleteChangeSetResponse 200
          , SendMatcher (isDeleteChangeSet cs2)
              $ Right
              $ newDeleteChangeSetResponse 200
          , SendMatcher (isDeleteChangeSet cs3)
              $ Right
              $ newDeleteChangeSetResponse 200
          ]

      withMatchers matchers $ do
        awsCloudFormationDeleteAllChangeSets $ StackName stackName

        messages <-
          map (loggedMessageText &&& loggedMessageMeta)
            <$> getLoggedMessagesUnsafe

        messages
          `shouldBe` [ ("Deleting all changesets", mempty)
                     , ("Enqueing delete", KeyMap.fromList [("changeSetId", toJSON cs1)])
                     , ("Enqueing delete", KeyMap.fromList [("changeSetId", toJSON cs2)])
                     , ("Enqueing delete", KeyMap.fromList [("changeSetId", toJSON cs3)])
                     ]
