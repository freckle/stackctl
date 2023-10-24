module Stackctl.AWS.EC2Spec
  ( spec
  ) where

import Stackctl.Test.App

import Amazonka.EC2.DescribeAvailabilityZones
import Amazonka.EC2.Types.AvailabilityZone
import Stackctl.AWS.EC2

spec :: Spec
spec = do
  describe "awsEc2DescribeFirstAvailabilityZoneRegionName" $ do
    it "returns the first AZ's region name" $ example $ runTestAppT $ do
      let
        zones =
          [ newAvailabilityZone & availabilityZone_regionName ?~ "us-east-1"
          , newAvailabilityZone & availabilityZone_regionName ?~ "us-east-2"
          , newAvailabilityZone & availabilityZone_regionName ?~ "us-west-1"
          ]
        matcher =
          SendMatcher (const @_ @DescribeAvailabilityZones True)
            $ Right
            $ newDescribeAvailabilityZonesResponse 200
            & describeAvailabilityZonesResponse_availabilityZones
            ?~ zones

      withMatcher matcher awsEc2DescribeFirstAvailabilityZoneRegionName
        `shouldReturn` "us-east-1"
