module Iso (tests) where

import Test.QuickCheck.Simple (Test, qcTest)

import Control.Applicative ((<$>))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson

import Data.JsonRpc (Request, Response, Error, ErrorStatus, genericToArrayJSON)

import Instances (Example)


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
  [ qcTest "iso - request" request
  , qcTest "iso - request array" requestA
  , qcTest "iso - error status" errorStatus
  , qcTest "iso - error object" errorObj
  , qcTest "iso - response" response ]
