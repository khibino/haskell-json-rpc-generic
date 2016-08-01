module Iso (tests) where

import Test.QuickCheck.Simple (Test, qcTest)

import Control.Applicative ((<$>))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson

import Data.JsonRpc (Request, Response, Error, ErrorStatus, genericToArrayJSON)
import qualified Data.JsonRpc.Failure as Error

import Instances (Example)

errorStatusCode :: ErrorStatus -> Bool
errorStatusCode s =
 Error.fromCode (Error.toCode s) == Just s

aesonED :: (Eq a, ToJSON a, FromJSON a)
        => a -> Bool
aesonED x = Aeson.decode (Aeson.encode x) == Just x

request :: Request Example -> Bool
request = aesonED

requestA :: Request Example -> Bool
requestA r =
  Aeson.decode (Aeson.encode $ genericToArrayJSON <$> r) == Just r

errorStatus :: ErrorStatus -> Bool
errorStatus = aesonED

errorObj :: Error Example -> Bool
errorObj = aesonED

response :: Response Example Example -> Bool
response = aesonED

tests :: [Test]
tests =
  [ qcTest "iso - error status code"  errorStatusCode
  ] ++

  [ x
  | x  <-  [ qcTest "iso JSON - error status" errorStatus ]
  ,  Aeson.decode (Aeson.encode (1 :: Integer)) == Just (1 :: Integer)
  ] ++
  [ qcTest "iso JSON - request" request
  , qcTest "iso JSON - request array" requestA
  , qcTest "iso JSON - error object" errorObj
  , qcTest "iso JSON - response" response ]
