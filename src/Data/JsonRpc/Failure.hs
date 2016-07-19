{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.JsonRpc.Failure (
  Failure (..), Error (..), ErrorStatus (..),

  failure, makeError,
  serverError,
  methodError,
  emptyError,
  ) where

import Prelude hiding (userError)
import Control.Monad (MonadPlus, guard)
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

makeError :: ErrorStatus -> Maybe Text -> Maybe e -> Error e
makeError e mm = d e $ fromMaybe (defaultMessage e) mm    where
  d  ParseError         =  Error (-32700)
  d  InvalidRequest     =  Error (-32600)
  d  MethodNotFound     =  Error (-32601)
  d  InvalidParams      =  Error (-32602)
  d  InternalError      =  Error (-32603)
  d (ServerError c)     =  Error       c
  d (MethodError c)     =  Error       c

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
