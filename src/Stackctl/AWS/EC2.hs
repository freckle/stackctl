module Stackctl.AWS.EC2
  ( awsEc2DescribeFirstAvailabilityZoneRegionName
  ) where

import Stackctl.Prelude

import Amazonka.EC2.DescribeAvailabilityZones
import Amazonka.EC2.Types (AvailabilityZone (..))
import Stackctl.AWS.Core as AWS

awsEc2DescribeFirstAvailabilityZoneRegionName
  :: (MonadIO m, MonadAWS m) => m Region
awsEc2DescribeFirstAvailabilityZoneRegionName = do
  let req = newDescribeAvailabilityZones
  AWS.simple req $ \resp -> do
    azs <- resp ^. describeAvailabilityZonesResponse_availabilityZones
    az <- listToMaybe azs
    rn <- regionName az
    hush $ fromText rn
