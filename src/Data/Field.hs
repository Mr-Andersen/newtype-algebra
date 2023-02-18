module Data.Field (Field, (/)) where

import Control.Newtype (unpack, pack)
import Data.Group (Group (inverse))
import Data.Ring (Ring)
import Data.Semiring (Semiring (Multiplicative), (*))
import Prelude qualified as P (Double, (/))

type Field a = (Ring a, Group (Multiplicative a))

(/) :: forall a. Field a => a -> a -> a
x / y = x * unpack @(Multiplicative a) (inverse (pack y))

{-# NOINLINE[2] (/) #-}
{-# RULES
    "Field./ @Double" [3] forall x y. (/) @P.Double x y = x P./ y;
  #-}
