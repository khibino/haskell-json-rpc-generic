
module Data.JsonRpc.Integral (
  fromScientific,
  ) where

import Control.Monad (MonadPlus, guard)
import Data.Scientific (Scientific, toDecimalDigits)


fromScientific :: MonadPlus m => Scientific -> m Integer
fromScientific sci = do
  let (ds, e) = toDecimalDigits sci
  guard $ sci == 0 || null (drop e ds)  -- test integral
  return $ round sci
