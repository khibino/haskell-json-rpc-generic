{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.JsonRpc.Failure (
  Failure (..), Error (..),
  ErrorStatus (..), toCode, fromCode,

  failure, makeError,
  serverError,
  methodError,
  emptyError,
  ) where

import Prelude hiding (userError)
import Control.Monad (MonadPlus, mplus, guard)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Data.JsonRpc.Id (Id)


data Failure e =
  Failure
  { _jsonrpc :: !Text
  , _id      :: !(Maybe Id)
  , _error   :: !(Error e)
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data Error e =
  Error
  { _code    :: !Int
  , _message :: !Text
  , _data    :: !(Maybe e)
  } deriving (Eq, Show, Functor, Foldable, Traversable)

{-
-- citation from http://www.jsonrpc.org/specification
--
-- The error codes from and including -32768 to -32000 are reserved for pre-defined errors.
-- Any code within this range, but not defined explicitly below is reserved for future use.
-- The error codes are nearly the same as those suggested for XML-RPC at the following
-- url: http://xmlrpc-epi.sourceforge.net/specs/rfc.fault_codes.php
--
-- code               message            meaning
-- -32700             Parse error        Invalid JSON was received by the server.
--                                       An error occurred on the server while parsing the JSON text.
-- -32600             Invalid Request    The JSON sent is not a valid Request object.
-- -32601             Method not found   The method does not exist / is not available.
-- -32602             Invalid params     Invalid method parameter(s).
-- -32603             Internal error     Internal JSON-RPC error.
-- -32000 to -32099   Server error       Reserved for implementation-defined server-errors.

-- The remainder of the space is available for application defined errors.
 -}
data ErrorStatus
  = ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  | ServerError !Int
  | MethodError !Int
  deriving (Eq, Show)

failure :: Maybe Id -> ErrorStatus -> Maybe Text -> Maybe e -> Failure e
failure mayId s mm =
  Failure "2.0" mayId . makeError s mm

defaultMessage :: ErrorStatus -> Text
defaultMessage = d  where
  d  ParseError         =  "Parse error"
  d  InvalidRequest     =  "Invalid Request"
  d  MethodNotFound     =  "Method not found"
  d  InvalidParams      =  "Invalid params"
  d  InternalError      =  "Internal error"
  d (ServerError _)     =  "Server error"
  d (MethodError _)     =  "Application method error"

toCode :: ErrorStatus -> Int
toCode = d  where
  d  ParseError         =  -32700
  d  InvalidRequest     =  -32600
  d  MethodNotFound     =  -32601
  d  InvalidParams      =  -32602
  d  InternalError      =  -32603
  d (ServerError c)     =       c
  d (MethodError c)     =       c

fromCode :: MonadPlus m => Int -> m ErrorStatus
fromCode c
  | c == -32700  =  return ParseError
  | c == -32600  =  return InvalidRequest
  | c == -32601  =  return MethodNotFound
  | c == -32602  =  return InvalidParams
  | c == -32603  =  return InternalError
  | otherwise    =  serverError c `mplus` methodError c

makeError :: ErrorStatus -> Maybe Text -> Maybe e -> Error e
makeError e = Error (toCode e) . fromMaybe (defaultMessage e)

serverError :: MonadPlus m
            => Int
            -> m ErrorStatus
serverError c = do
  guard $ -32099 <= c && c <= -32000
  return $ ServerError c

methodError :: MonadPlus m
            => Int
            -> m ErrorStatus
methodError c = do
  guard $ c < -32768 || -32000 < c
  return $ MethodError c

emptyError :: Maybe ()
emptyError = Nothing
