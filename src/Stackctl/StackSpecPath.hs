module Stackctl.StackSpecPath
  ( StackSpecPath

  -- * Fields
  , stackSpecPathAccountId
  , stackSpecPathAccountName
  , stackSpecPathRegion
  , stackSpecPathStackName
  , stackSpecPathBasePath
  , stackSpecPathFilePath

  -- * Construction
  , stackSpecPath
  , stackSpecPathFromFilePath
  ) where

import Stackctl.Prelude

import Stackctl.AWS
import RIO.FilePath (joinPath, splitDirectories)
import qualified RIO.Text as T
import System.FilePath.Glob

data StackSpecPath = StackSpecPath
  { sspAccountId :: AccountId
  , sspAccountName :: Text
  , sspRegion :: Region
  , sspStackName :: StackName
  , sspPath :: FilePath
  }

instance Display StackSpecPath where
  display StackSpecPath {..} =
    display sspAccountId
      <> " ("
      <> display sspAccountName
      <> ") "
      <> display sspRegion
      <> ": "
      <> display sspStackName

stackSpecPath
  :: AccountId -> Text -> Region -> StackName -> FilePath -> StackSpecPath
stackSpecPath = StackSpecPath

stackSpecPathAccountId :: StackSpecPath -> AccountId
stackSpecPathAccountId = sspAccountId

stackSpecPathAccountName :: StackSpecPath -> Text
stackSpecPathAccountName = sspAccountName

stackSpecPathRegion :: StackSpecPath -> Region
stackSpecPathRegion = sspRegion

stackSpecPathBasePath :: StackSpecPath -> FilePath
stackSpecPathBasePath = sspPath

stackSpecPathStackName :: StackSpecPath -> StackName
stackSpecPathStackName = sspStackName

-- | Render the (relative) 'StackSpecPath'
stackSpecPathFilePath :: StackSpecPath -> FilePath
stackSpecPathFilePath StackSpecPath {..} =
  "stacks"
    </> unpack (unAccountId sspAccountId <> "." <> sspAccountName)
    </> unpack (fromRegion sspRegion)
    </> sspPath

stackSpecPathFromFilePath
  :: AccountId
  -> Region
  -> FilePath -- ^ Must be relative, @stacks/@
  -> Either String StackSpecPath
stackSpecPathFromFilePath accountId region path = case splitDirectories path of
  ("stacks" : pathAccount : pathRegion : rest) -> do
    let
      accountName = T.drop 1 $ T.dropWhile (/= '.') $ pack pathAccount
      accountPattern = compile $ unpack (unAccountId accountId) <> ".*"

    unless (accountPattern `match` pathAccount)
      $ Left
      $ "Unexpected account: "
      <> pathAccount
      <> " !=~ "
      <> decompile accountPattern

    unless (unpack (fromRegion region) == pathRegion)
      $ Left
      $ "Unexpected region: "
      <> pathRegion
      <> " != "
      <> unpack (fromRegion region)

    stackName <-
      maybe (Left "Must end in .yaml") (Right . StackName)
      $ T.stripSuffix ".yaml"
      $ T.intercalate "-"
      $ map pack rest

    Right $ StackSpecPath
      { sspAccountId = accountId
      , sspAccountName = accountName
      , sspRegion = region
      , sspStackName = stackName
      , sspPath = joinPath rest
      }

  _ -> Left $ "Path is not stacks/././.: " <> path
