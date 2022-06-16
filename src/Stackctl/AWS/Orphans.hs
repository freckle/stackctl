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

import Amazonka.CloudFormation.DescribeChangeSet
import Amazonka.CloudFormation.Types
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
