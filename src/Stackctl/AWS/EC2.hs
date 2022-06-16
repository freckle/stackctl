module Stackctl.AWS.EC2
  ( awsEc2DescribeFirstAvailabilityZoneRegionName
  ) where

import Stackctl.Prelude2

import Amazonka.EC2.DescribeAvailabilityZones
import Amazonka.EC2.Types (AvailabilityZone(..))
import RIO.List (headMaybe)
import Stackctl.AWS.Core

awsEc2DescribeFirstAvailabilityZoneRegionName
  :: (MonadResource m, MonadReader env m, HasAwsEnv env) => m Region
awsEc2DescribeFirstAvailabilityZoneRegionName = do
  let req = newDescribeAvailabilityZones
  awsSimple "DescribeAvailabilityZones" req $ \resp -> do
    azs <- resp ^. describeAvailabilityZonesResponse_availabilityZones
    az <- headMaybe azs
    rn <- regionName az
    hush $ fromText rn
