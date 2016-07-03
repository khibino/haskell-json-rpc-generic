{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Test.QuickCheck.Simple (boolTest, defaultMain)

import GHC.Generics (Generic)

import Prelude hiding (id)
import Data.Maybe (fromMaybe)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Data.JsonRpc.Id (Id(..), numberId)
import Data.JsonRpc.Request (Request (..))
import Data.JsonRpc.Success (Success, success)
import Data.JsonRpc.Failure (Failure, failure)
import Data.JsonRpc.Response (Response (..))
import qualified Data.JsonRpc.Failure as Failure
import Data.JsonRpc.Instances ()
import Data.JsonRpc.Generic (genericParseJSONRPC)


data Foo =
  Foo
  { foo :: Int
  , bar :: String
  , baz :: [Int]
  } deriving (Eq, Show, Generic)

instance FromJSON Foo where
  parseJSON = genericParseJSONRPC Aeson.defaultOptions


eqDecode0 :: Bool
eqDecode0 =
  Aeson.decode
  "{\"jsonrpc\": \"2.0\", \"method\": \"foo\", \"params\": { \"bar\": \"Hello\", \"foo\": 234, \"baz\": [5, 6, 7, 8] }, \"id\": 3}"
  ==
  Just (Request { jsonrpc = "2.0"
                , method = "foo"
                , params = Just (Foo {foo = 234, bar = "Hello", baz = [5,6,7,8]})
                , id = Just (NumberId 3)}
       )

eqDecode1 :: Bool
eqDecode1 =
  Aeson.decode
  "{\"jsonrpc\": \"2.0\", \"method\": \"foo\", \"params\": [ 234, \"Hello\", [5, 6, 7, 8] ], \"id\": 3}"
  ==
  Just (Request { jsonrpc = "2.0"
                , method = "foo"
                , params = Just (Foo {foo = 234, bar = "Hello", baz = [5,6,7,8]})
                , id = Just (NumberId 3)}
       )

exId :: Id
exId = fromMaybe (error "something wrong: _success") $ numberId 25

exSuccess :: Success String
exSuccess = success exId ("World!" :: String)

exFailure :: Failure String
exFailure = failure (Just exId) Failure.InvalidRequest Nothing

eqResponseS :: Bool
eqResponseS =
  Aeson.encode (Response $ Right exSuccess :: Response String String)
  ==
  "{\"result\":\"World!\",\"jsonrpc\":\"2.0\",\"id\":25}"

eqResponseF :: Bool
eqResponseF =
  Aeson.encode (Response $ Left exFailure :: Response String String)
  ==
  "{\"error\":{\"code\":-32600,\"message\":\"Invalid Request\"},\"jsonrpc\":\"2.0\",\"id\":25}"

main :: IO ()
main =
  defaultMain
  [ boolTest "decode 0" eqDecode0
  , boolTest "decode 1" eqDecode1
  , boolTest "response success" eqResponseS
  , boolTest "response failure" eqResponseF
  ]
