module Stackctl.AWS.CertificateManager
  ( CertificateSummary(..)
  , awsAcmListCertificates
  ) where

import Stackctl.Prelude

import Amazonka.CertificateManager.ListCertificates
import Amazonka.CertificateManager.Types (CertificateSummary(..))
import Stackctl.AWS.Core

-- NB. We don't expect enough to exist to require paginating
awsAcmListCertificates
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
  => m [CertificateSummary]
awsAcmListCertificates = awsSimple
  "ListCertificates"
  newListCertificates
  (^. listCertificatesResponse_certificateSummaryList)
