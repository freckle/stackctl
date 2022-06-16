{-# LANGUAGE NamedFieldPuns #-}

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

import Stackctl.Prelude2

import RIO.Char (isDigit)
import RIO.FilePath (joinPath, splitDirectories)
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (breakOn)
import Stackctl.AWS

data StackSpecPath = StackSpecPath
  { sspAccountId :: AccountId
  , sspAccountName :: Text
  , sspAccountPathPart :: FilePath
  , sspRegion :: Region
  , sspStackName :: StackName
  , sspPath :: FilePath
  }

stackSpecPath
  :: AccountId -> Text -> Region -> StackName -> FilePath -> StackSpecPath
stackSpecPath sspAccountId sspAccountName sspRegion sspStackName sspPath =
  StackSpecPath
    { sspAccountId
    , sspAccountName
    , sspAccountPathPart
    , sspRegion
    , sspStackName
    , sspPath
    }
 where
  sspAccountPathPart =
    unpack $ unAccountId sspAccountId <> "." <> sspAccountName

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
  "stacks" </> sspAccountPathPart </> unpack (fromRegion sspRegion) </> sspPath

stackSpecPathFromFilePath
  :: AccountId
  -> Region
  -> FilePath -- ^ Must be relative, @stacks/@
  -> Either String StackSpecPath
stackSpecPathFromFilePath accountId region path = case splitDirectories path of
  ("stacks" : pathAccount : pathRegion : rest) -> do
    (accountName, pathAccountId) <- parseAccountPath pathAccount

    unless (pathAccountId == accountId)
      $ Left
      $ "Unexpected account: "
      <> unpack (unAccountId pathAccountId)
      <> " != "
      <> unpack (unAccountId accountId)

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
      , sspAccountPathPart = pathAccount
      , sspRegion = region
      , sspStackName = stackName
      , sspPath = joinPath rest
      }

  _ -> Left $ "Path is not stacks/././.: " <> path

-- | Handle @{account-name}.{account-id}@ or @{account-id}.{account-name}@
parseAccountPath :: FilePath -> Either String (Text, AccountId)
parseAccountPath path = case second (T.drop 1) $ T.breakOn "." $ pack path of
  (a, b) | isAccountId a -> Right (b, AccountId a)
  (a, b) | isAccountId b -> Right (a, AccountId b)
  _ ->
    Left
      $ "Path matches neither {account-id}.{account-name}, nor {account-name}.{account-id}: "
      <> path
  where isAccountId x = T.length x == 12 && T.all isDigit x
