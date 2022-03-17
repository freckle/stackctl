module Stackctl.AWS.EC2
  ( awsEc2DescribeFirstAvailabilityZoneRegionName
  ) where

import Stackctl.Prelude

import Amazonka.EC2.DescribeAvailabilityZones
import Amazonka.EC2.Types (AvailabilityZone(..))
import Stackctl.AWS.Core
import RIO.List (headMaybe)

awsEc2DescribeFirstAvailabilityZoneRegionName
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
  => m Region
awsEc2DescribeFirstAvailabilityZoneRegionName = do
  let req = newDescribeAvailabilityZones
  awsSimple "DescribeAvailabilityZones" req $ \resp -> do
    azs <- resp ^. describeAvailabilityZonesResponse_availabilityZones
    az <- headMaybe azs
    rn <- regionName az
    hush $ fromText rn
