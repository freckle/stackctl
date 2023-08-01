{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
--
-- Orphans so we can get @'ToJSON' 'ChangeSet'@ without hand-writing a massive,
-- incomplete, and doomed-to-drift instance ourselves.
module Stackctl.AWS.Orphans () where

import Stackctl.Prelude

import Amazonka.CloudFormation.DescribeChangeSet
import Amazonka.CloudFormation.Types
import Data.Aeson
import GHC.Generics (Rep)

-- Makes it syntactally easier to do a bunch of these
newtype Generically a = Generically {unGenerically :: a}

instance
  ( Generic a
  , GFromJSON Zero (Rep a)
  )
  => FromJSON (Generically a)
  where
  parseJSON = fmap Generically . genericParseJSON defaultOptions

instance
  ( Generic a
  , GToJSON' Value Zero (Rep a)
  , GToJSON' Encoding Zero (Rep a)
  )
  => ToJSON (Generically a)
  where
  toJSON = genericToJSON defaultOptions . unGenerically
  toEncoding = genericToEncoding defaultOptions . unGenerically

{- FOURMOLU_DISABLE -}

deriving via (Generically Change) instance FromJSON Change
deriving via (Generically Change) instance ToJSON Change
deriving via (Generically DescribeChangeSetResponse) instance FromJSON DescribeChangeSetResponse
deriving via (Generically DescribeChangeSetResponse) instance ToJSON DescribeChangeSetResponse
deriving via (Generically ModuleInfo) instance FromJSON ModuleInfo
deriving via (Generically ModuleInfo) instance ToJSON ModuleInfo
deriving via (Generically Parameter) instance FromJSON Parameter
deriving via (Generically Parameter) instance ToJSON Parameter
deriving via (Generically ResourceChange) instance FromJSON ResourceChange
deriving via (Generically ResourceChange) instance ToJSON ResourceChange
deriving via (Generically ResourceChangeDetail) instance FromJSON ResourceChangeDetail
deriving via (Generically ResourceChangeDetail) instance ToJSON ResourceChangeDetail
deriving via (Generically ResourceTargetDefinition) instance FromJSON ResourceTargetDefinition
deriving via (Generically ResourceTargetDefinition) instance ToJSON ResourceTargetDefinition
deriving via (Generically RollbackConfiguration) instance FromJSON RollbackConfiguration
deriving via (Generically RollbackConfiguration) instance ToJSON RollbackConfiguration
deriving via (Generically RollbackTrigger) instance FromJSON RollbackTrigger
deriving via (Generically RollbackTrigger) instance ToJSON RollbackTrigger
deriving via (Generically Tag) instance FromJSON Tag
deriving via (Generically Tag) instance ToJSON Tag
