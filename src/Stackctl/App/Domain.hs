module Stackctl.App.Domain
  ( Domain(..)
  , CertificateArn(..)
  , fetchCertificateArnByDomain
  ) where

import Stackctl.Prelude

import Data.List.Extra (firstJust)
import Stackctl.AWS
import qualified RIO.Text as T

newtype Domain = Domain
    { unDomain :: Text
    }
    deriving newtype Display

newtype CertificateArn = CertificateArn
    { unCertificateArn :: Text
    }

fetchCertificateArnByDomain
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
  => Domain
  -> m CertificateArn
fetchCertificateArnByDomain domain = do
  logDebug $ "Searching Certificates for *." <> display domain
  certificateSummaries <- awsAcmListCertificates

  let
    certificateNotFound =
      throwString
        $ "No Certificate found for *."
        <> unpack (unDomain domain)
        <> " in "
        <> show certificateSummaries

  maybe certificateNotFound pure $ firstJust matchToArn certificateSummaries
 where
  matchToArn summary = do
    arn <- certificateArn summary
    apex <- T.stripPrefix "*." =<< domainName summary
    CertificateArn arn <$ guard (apex == unDomain domain)
