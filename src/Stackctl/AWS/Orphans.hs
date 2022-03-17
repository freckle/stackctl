{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
--
-- Orphans so we can get @'ToJSON' 'ChangeSet'@ without hand-writing a massive,
-- incomplete, and doomed-to-drift instance ourselves.
--
module Stackctl.AWS.Orphans
  () where

import Stackctl.Prelude

import Amazonka (Region, ToText(..))
import Amazonka.CloudFormation.DescribeChangeSet
import Amazonka.CloudFormation.Types
import Amazonka.S3.Types (BucketName, ObjectKey)
import Data.Aeson
import GHC.Generics (Rep)

-- Makes it syntactally easier to do a bunch of these
newtype Generically a = Generically { unGenerically :: a }
instance
  ( Generic a
  , GToJSON' Value Zero (Rep a)
  , GToJSON' Encoding Zero (Rep a)
  ) => ToJSON (Generically a) where
  toJSON = genericToJSON defaultOptions . unGenerically
  toEncoding = genericToEncoding defaultOptions . unGenerically

deriving via (Generically DescribeChangeSetResponse)
  instance ToJSON DescribeChangeSetResponse
deriving via (Generically Tag)
  instance ToJSON Tag
deriving via (Generically Parameter)
  instance ToJSON Parameter
deriving via (Generically RollbackConfiguration)
  instance ToJSON RollbackConfiguration
deriving via (Generically RollbackTrigger)
  instance ToJSON RollbackTrigger
deriving via (Generically Change)
  instance ToJSON Change
deriving via (Generically ResourceChange)
  instance ToJSON ResourceChange
deriving via (Generically ModuleInfo)
  instance ToJSON ModuleInfo
deriving via (Generically ResourceChangeDetail)
  instance ToJSON ResourceChangeDetail
deriving via (Generically ResourceTargetDefinition)
  instance ToJSON ResourceTargetDefinition

newtype ToTextable a = ToTextable { unToTextable :: a }
instance (ToText a) => Display (ToTextable a) where
  display = display . toText . unToTextable

deriving via (ToTextable Region)
  instance Display Region

deriving via (ToTextable Capability)
  instance Display Capability
deriving via (ToTextable ChangeAction)
  instance Display ChangeAction
deriving via (ToTextable ChangeSource)
  instance Display ChangeSource
deriving via (ToTextable Replacement)
  instance Display Replacement
deriving via (ToTextable ResourceAttribute)
  instance Display ResourceAttribute
deriving via (ToTextable RequiresRecreation)
  instance Display RequiresRecreation

deriving via (ToTextable BucketName)
  instance Display BucketName
deriving via (ToTextable ObjectKey)
  instance Display ObjectKey
