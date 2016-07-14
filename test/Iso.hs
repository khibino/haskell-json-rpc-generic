module Iso where

import Test.QuickCheck.Simple (Test, qcTest)

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson

import Data.JsonRpc.Response (Response)
import Data.JsonRpc.Instances ()

import Instances (Example (..))


aesonED :: (Eq a, ToJSON a, FromJSON a)
        => a -> Bool
aesonED x = Aeson.decode (Aeson.encode x) == Just x

response :: Response Example Example -> Bool
response = aesonED

tests :: [Test]
tests =
  [ qcTest "iso - response" response ]
