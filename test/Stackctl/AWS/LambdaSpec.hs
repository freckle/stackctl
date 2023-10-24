module Stackctl.AWS.LambdaSpec
  ( spec
  ) where

import Stackctl.Test.App

import Amazonka.Lambda.Invoke
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Stackctl.AWS.Lambda

spec :: Spec
spec = do
  describe "awsLambdaInvoke" $ do
    it "invokes a lambda" $ example $ runTestAppT $ do
      let
        emptyObject = object []

        isInvocation name invoke =
          and
            [ invoke ^. invoke_functionName == name
            , invoke ^. invoke_payload == "{}"
            ]

        lambdaError =
          LambdaError
            { errorType = "exception"
            , errorMessage = "oops"
            , trace = []
            }

        matchers =
          [ SendMatcher (isInvocation "lambda-1")
              $ Right
              $ newInvokeResponse 200
              & invokeResponse_payload
              ?~ "<response>"
          , SendMatcher (isInvocation "lambda-2")
              $ Right
              $ newInvokeResponse 200
              & invokeResponse_payload
              ?~ BSL.toStrict (encode lambdaError)
          , SendMatcher (isInvocation "lambda-3")
              $ Right
              $ newInvokeResponse 500
              & (invokeResponse_payload ?~ "<response>")
              . (invokeResponse_functionError ?~ "<error>")
          ]

      withMatchers matchers $ do
        LambdaInvokeSuccess successPayload <-
          awsLambdaInvoke "lambda-1" emptyObject

        successPayload `shouldBe` "<response>"

        LambdaInvokeError errorPayload _ <-
          awsLambdaInvoke "lambda-2" emptyObject

        errorPayload `shouldBe` lambdaError

        LambdaInvokeFailure failureStatus failureFunctionError <-
          awsLambdaInvoke "lambda-3" emptyObject

        failureStatus `shouldBe` 500
        failureFunctionError `shouldBe` Just "<error>"
