module Data.Ring (Ring, (-)) where

import Control.Newtype (unpack, pack)
import Data.Group (Group (inverse))
import Data.Semiring (Semiring (Additive), (+))

type Ring a = (Semiring a, Group (Additive a))

(-) :: forall a. Ring a => a -> a -> a
x - y = x + unpack @(Additive a) (inverse (pack y))
