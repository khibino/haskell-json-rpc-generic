
module Data.JsonRpc.Id (Id(..), numberId) where

import Control.Applicative (Applicative, (<$>))
import Control.Monad (MonadPlus)
import Data.Text (Text)
import Data.Scientific (Scientific)

import Data.JsonRpc.Integral (fromScientific)

{-
-- citation from http://www.jsonrpc.org/specification
--
--  An identifier established by the Client that MUST contain
--  a String, Number, or NULL value if included.
--  If it is not included it is assumed to be a notification.
--  The value SHOULD normally not be Null [1]
--  and Numbers SHOULD NOT contain fractional parts [2]
--
--  [1] The use of Null as a value for the id member in
--  a Request object is discouraged, because this specification
--  uses a value of Null for Responses with an unknown id.
--  Also, because JSON-RPC 1.0 uses an id value of Null
--  for Notifications this could cause confusion in handling.

--  [2] Fractional parts may be problematic,
--  since many decimal fractions cannot be
--  represented exactly as binary fractions.
-}

data Id
  = StringId !Text
  | NumberId !Integer
  deriving (Eq, Ord, Show, Read)

numberId :: (MonadPlus m, Applicative m) => Scientific -> m Id
numberId sci = NumberId <$> fromScientific sci
