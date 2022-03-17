module Stackctl.AWS.S3
  (
  -- * Name
    BucketName(..)
  , unBucketName

  -- * Prefixes
  , BucketPathPrefix
  , bucketPathPrefix
  , unBucketPathPrefix

  -- * Absolute Keys
  , ObjectKey
  , objectKey
  , unObjectKey

  -- * @aws s3 cp@
  , S3CpDryRun(..)
  , awsS3Cp
  ) where

import Stackctl.Prelude

import Amazonka.Data.Body (chunkedFile, defaultChunkSize)
import Amazonka.S3 (BucketName(..), ObjectKey(..))
import Amazonka.S3.PutObject
import Data.Aeson
import Stackctl.AWS.Core
import qualified RIO.Text as T

unBucketName :: BucketName -> Text
unBucketName (BucketName x) = x

newtype BucketPathPrefix = BucketPathPrefix Text
  deriving stock (Show, Eq)

instance FromJSON BucketPathPrefix where
  parseJSON = fmap bucketPathPrefix . parseJSON

bucketPathPrefix :: Text -> BucketPathPrefix
bucketPathPrefix x = BucketPathPrefix $ stripSlashes x <> "/"
  where stripSlashes = T.dropWhile (== '/') . T.dropWhileEnd (== '/')

unBucketPathPrefix :: BucketPathPrefix -> Text
unBucketPathPrefix (BucketPathPrefix x) = x

objectKey :: Maybe BucketPathPrefix -> Text -> ObjectKey
objectKey mPrefix path =
  ObjectKey $ maybe "" unBucketPathPrefix mPrefix <> T.dropWhile (== '/') path

unObjectKey :: ObjectKey -> Text
unObjectKey (ObjectKey x) = x

data S3CpDryRun = S3CpDryRun | S3CpActual
  deriving stock Eq

awsS3Cp
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
  => S3CpDryRun
  -> FilePath
  -> BucketName
  -> ObjectKey
  -> m ()
awsS3Cp dryRun path bucket key = do
  logInfo
    $ "Upload: "
    <> fromString path
    <> " to "
    <> "s3://"
    <> display bucket
    <> "/"
    <> display key

  case dryRun of
    S3CpDryRun -> logInfo "Skipping (dryrun)"
    S3CpActual -> do
      body <- chunkedFile defaultChunkSize path
      status <-
        awsSimple "PutObject" (newPutObject bucket key body)
        $ pure
        . (^. putObjectResponse_httpStatus)

      if status == 200
        then logInfo "Success"
        else logWarn $ "Status: " <> display status
