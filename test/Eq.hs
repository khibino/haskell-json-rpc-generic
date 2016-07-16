{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Eq (tests) where

import Test.QuickCheck.Simple (Test, eqTest)

import GHC.Generics (Generic)

import Prelude hiding (id)
import Data.Maybe (fromMaybe)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Data.JsonRpc
  (Id(..), numberId, Request (..),
   Success, success, Failure, failure, Response (..),
  genericParseJSONRPC, defaultJsonRpcOptions, )
import qualified Data.JsonRpc.Failure as Failure


data Foo =
  Foo
  { foo :: Int
  , bar :: String
  , baz :: [Int]
  } deriving (Eq, Show, Generic)

instance FromJSON Foo where
  parseJSON = genericParseJSONRPC defaultJsonRpcOptions Aeson.defaultOptions

eqDecodeO :: Test
eqDecodeO =
  eqTest "eq - decode object"
  (Aeson.decode
   "{\"jsonrpc\": \"2.0\", \"method\": \"foo\", \"params\": { \"bar\": \"Hello\", \"foo\": 234, \"baz\": [5, 6, 7, 8] }, \"id\": 3}")
  (Just Request { jsonrpc = "2.0"
                , method = "foo"
                , params = Just (Foo {foo = 234, bar = "Hello", baz = [5,6,7,8]})
                , id = Just (NumberId 3)}
  )

eqDecodeA :: Test
eqDecodeA =
  eqTest "eq - decode array"
  (Aeson.decode
   "{\"jsonrpc\": \"2.0\", \"method\": \"foo\", \"params\": [ 234, \"Hello\", [5, 6, 7, 8] ], \"id\": 3}")
  (Just Request { jsonrpc = "2.0"
                , method = "foo"
                , params = Just (Foo {foo = 234, bar = "Hello", baz = [5,6,7,8]})
                , id = Just (NumberId 3)}
  )

exId :: Id
exId = fromMaybe (error "something wrong: _success") $ numberId 25

exSuccess :: Success Foo
exSuccess = success exId Foo {foo = 234, bar = "Hello", baz = [5,6,7,8]}

exFailure :: Failure String
exFailure = failure (Just exId) Failure.InvalidRequest Nothing

eqResponseS :: Test
eqResponseS =
  eqTest "eq - response success"
  (Just (Response $ Right exSuccess :: Response String Foo))
  (Aeson.decode "{\"result\":{ \"bar\": \"Hello\", \"foo\": 234, \"baz\": [5, 6, 7, 8] },\"jsonrpc\":\"2.0\",\"id\":25}")

eqResponseF :: Test
eqResponseF =
  eqTest "eq - response failure"
  (Just (Response $ Left exFailure :: Response String Foo))
  (Aeson.decode "{\"error\":{\"code\":-32600,\"message\":\"Invalid Request\"},\"jsonrpc\":\"2.0\",\"id\":25}")

tests :: [Test]
tests =
  [ eqDecodeO
  , eqDecodeA
  , eqResponseS
  , eqResponseF
  ]
