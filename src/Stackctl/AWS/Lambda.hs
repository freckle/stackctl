{-# LANGUAGE MultiWayIf #-}

module Stackctl.AWS.Lambda
  ( LambdaInvokeResult (..)
  , LambdaError (..)
  , logLambdaInvocationResult
  , isLambdaInvocationSuccess
  , awsLambdaInvoke
  ) where

import Stackctl.Prelude hiding (trace)

import Amazonka.Lambda.Invoke
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Stackctl.AWS.Core

data LambdaInvokeResult
  = LambdaInvokeSuccess ByteString
  | LambdaInvokeError LambdaError (Maybe Text)
  | LambdaInvokeFailure Int (Maybe Text)
  deriving stock (Show)

logLambdaInvocationResult :: MonadLogger m => LambdaInvokeResult -> m ()
logLambdaInvocationResult = \case
  LambdaInvokeSuccess bs -> do
    let meta = case decode @Value $ BSL.fromStrict bs of
          Nothing -> ["response" .= decodeUtf8 bs]
          Just response -> ["response" .= response]
    logInfo $ "LambdaInvokeSuccess" :# meta
  LambdaInvokeError LambdaError {..} mFunctionError ->
    logError
      $ (:# [])
      $ mconcat
        [ "LambdaInvokeError"
        , "\n  errorType: " <> errorType
        , "\n  errorMessage: " <> errorMessage
        , "\n  trace: "
        , mconcat $ map ("\n    " <>) trace
        , "\n  FunctionError: " <> fromMaybe "none" mFunctionError
        ]
  LambdaInvokeFailure status mFunctionError ->
    logError
      $ (:# [])
      $ mconcat
        [ "LambdaInvokeFailure"
        , "\n  StatusCode: " <> pack (show status)
        , "\n  FunctionError: " <> fromMaybe "none" mFunctionError
        ]

isLambdaInvocationSuccess :: LambdaInvokeResult -> Bool
isLambdaInvocationSuccess = \case
  LambdaInvokeSuccess {} -> True
  LambdaInvokeError {} -> False
  LambdaInvokeFailure {} -> False

data LambdaError = LambdaError
  { errorType :: Text
  , errorMessage :: Text
  , trace :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

awsLambdaInvoke
  :: ( MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasAwsEnv env
     , ToJSON a
     )
  => Text
  -> a
  -- ^ Payload
  -> m LambdaInvokeResult
awsLambdaInvoke name payload = do
  logDebug $ "Invoking function" :# ["name" .= name]

  -- Match Lambda's own limit (15 minutes) and add some buffer
  resp <-
    awsTimeout 905
      $ awsSend
      $ newInvoke name
      $ BSL.toStrict
      $ encode
        payload

  let
    status = resp ^. invokeResponse_statusCode
    mError = decode . BSL.fromStrict =<< resp ^. invokeResponse_payload
    mFunctionError = resp ^. invokeResponse_functionError
    response = fromMaybe "" $ resp ^. invokeResponse_payload

  logDebug
    $ "Function result"
    :# [ "name" .= name
       , "status" .= status
       , "error" .= mError
       , "functionError" .= mFunctionError
       ]

  pure
    $ if
      | statusIsUnsuccessful status -> LambdaInvokeFailure status mFunctionError
      | Just e <- mError -> LambdaInvokeError e mFunctionError
      | otherwise -> LambdaInvokeSuccess response

statusIsUnsuccessful :: Int -> Bool
statusIsUnsuccessful s = s < 200 || s >= 300
