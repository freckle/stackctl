module Stackctl.AWS.SSM
  ( awsGetParameterValue
  ) where

import Stackctl.Prelude

import Amazonka.SSM (_ParameterNotFound)
import Amazonka.SSM.GetParameter
import Amazonka.SSM.Types.Parameter
import Stackctl.AWS.Core as AWS
import UnliftIO.Exception.Lens (handling_)

awsGetParameterValue
  :: (MonadUnliftIO m, MonadAWS m)
  => Text
  -> m (Maybe Text)
awsGetParameterValue name =
  handling_ _ParameterNotFound (pure Nothing) $ fmap Just $ do
    resp <- AWS.send $ newGetParameter name
    pure $ resp ^. getParameterResponse_parameter . parameter_value
