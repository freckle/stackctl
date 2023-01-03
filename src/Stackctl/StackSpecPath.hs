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

import Stackctl.Prelude

import Data.Char (isDigit)
import qualified Data.Text as T
import Stackctl.AWS
import Stackctl.AWS.Scope
import System.FilePath (joinPath, splitDirectories)

data StackSpecPath = StackSpecPath
  { sspAwsScope :: AwsScope
  , sspAccountPathPart :: FilePath
  , sspStackName :: StackName
  , sspPath :: FilePath
  }
  deriving stock (Eq, Show)

stackSpecPath :: AwsScope -> StackName -> FilePath -> StackSpecPath
stackSpecPath sspAwsScope@AwsScope {..} sspStackName sspPath = StackSpecPath
  { sspAwsScope
  , sspAccountPathPart
  , sspStackName
  , sspPath
  }
 where
  sspAccountPathPart =
    unpack $ unAccountId awsAccountId <> "." <> awsAccountName

stackSpecPathAccountId :: StackSpecPath -> AccountId
stackSpecPathAccountId = awsAccountId . sspAwsScope

stackSpecPathAccountName :: StackSpecPath -> Text
stackSpecPathAccountName = awsAccountName . sspAwsScope

stackSpecPathAccountPathPart :: StackSpecPath -> FilePath
stackSpecPathAccountPathPart = sspAccountPathPart

stackSpecPathRegion :: StackSpecPath -> Region
stackSpecPathRegion = awsRegion . sspAwsScope

stackSpecPathBasePath :: StackSpecPath -> FilePath
stackSpecPathBasePath = sspPath

stackSpecPathStackName :: StackSpecPath -> StackName
stackSpecPathStackName = sspStackName

-- | Render the (relative) 'StackSpecPath'
stackSpecPathFilePath :: StackSpecPath -> FilePath
stackSpecPathFilePath path =
  "stacks"
    </> stackSpecPathAccountPathPart path
    </> unpack (fromRegion $ stackSpecPathRegion path)
    </> stackSpecPathBasePath path

stackSpecPathFromFilePath
  :: AwsScope
  -> FilePath -- ^ Must be relative, @stacks/@
  -> Either String StackSpecPath
stackSpecPathFromFilePath awsScope@AwsScope {..} path =
  case splitDirectories path of
    ("stacks" : pathAccount : pathRegion : rest) -> do
      (accountName, pathAccountId) <- parseAccountPath pathAccount

      unless (pathAccountId == awsAccountId)
        $ Left
        $ "Unexpected account: "
        <> unpack (unAccountId pathAccountId)
        <> " != "
        <> unpack (unAccountId awsAccountId)

      unless (unpack (fromRegion awsRegion) == pathRegion)
        $ Left
        $ "Unexpected region: "
        <> pathRegion
        <> " != "
        <> unpack (fromRegion awsRegion)

      stackName <-
        maybe (Left "Must end in .yaml") (Right . StackName)
        $ T.stripSuffix ".yaml"
        $ T.intercalate "-"
        $ map pack rest

      Right $ StackSpecPath
        { sspAwsScope = awsScope { awsAccountName = accountName }
        , sspAccountPathPart = pathAccount
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
