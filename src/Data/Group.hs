module Data.Group (Group(inverse)) where

import Control.Newtype (over)
import Data.Monoid (Monoid, Sum (Sum), Product (Product))
import Prelude (Fractional, Num, negate, (/))

class Monoid a => Group a where
  -- | forall x. x <> inverse x = inverse x <> x = mempty
  inverse :: a -> a

instance Num a => Group (Sum a) where
  inverse = over Sum negate

instance Fractional a => Group (Product a) where
  inverse = over Product (1/)
