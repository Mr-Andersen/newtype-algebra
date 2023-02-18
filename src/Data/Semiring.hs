{-# LANGUAGE TypeFamilies #-}

module Data.Semiring (Semiring (Additive, Multiplicative), SwitchSemiring (SwitchSemiring, getSwitchSemiring), zero, one, (+), (*)) where

import Control.Category ((.))
import Control.Newtype (Newtype (unpack, pack))
import Data.Kind (Type)
import Data.Monoid (Monoid (mempty), (<>), Any, All, Sum, Product)
import Data.Ratio (Ratio)
import Data.Semigroup (Semigroup)
import Prelude (Bool, Double, Integer, Num, Show)

class
  ( Monoid (Additive a)
  , Newtype (Additive a) a
  , Monoid (Multiplicative a)
  , Newtype (Multiplicative a) a
  ) => Semiring a
  where
    type Additive a :: Type
    type Multiplicative a :: Type

zero :: forall a. Semiring a => a
zero = unpack @(Additive a) mempty

(+) :: forall a. Semiring a => a -> a -> a
x + y = unpack @(Additive a) (pack x <> pack y)

one :: forall a. Semiring a => a
one = unpack @(Multiplicative a) mempty

(*) :: forall a. Semiring a => a -> a -> a
x * y = unpack @(Multiplicative a) (pack x <> pack y)

instance Semiring Bool where
  type Additive Bool = Any
  type Multiplicative Bool = All

instance Semiring Integer where
  type Additive Integer = Sum Integer
  type Multiplicative Integer = Product Integer

instance Semiring (Ratio Integer) where
  type Additive (Ratio Integer) = Sum (Ratio Integer)
  type Multiplicative (Ratio Integer) = Product (Ratio Integer)

instance Semiring Double where
  type Additive Double = Sum Double
  type Multiplicative Double = Product Double

-- | Magic newtype that switches Additive and Multiplicative bahavior for `a`
newtype SwitchSemiring a = SwitchSemiring { getSwitchSemiring :: a }
  deriving newtype (Semigroup, Monoid, Num, Show)

instance Newtype n a => Newtype (SwitchSemiring n) (SwitchSemiring a) where
  pack = SwitchSemiring . pack . getSwitchSemiring
  unpack = SwitchSemiring . unpack . getSwitchSemiring

instance Semiring a => Semiring (SwitchSemiring a) where
  type Additive (SwitchSemiring a) = SwitchSemiring (Multiplicative a)
  type Multiplicative (SwitchSemiring a) = SwitchSemiring (Additive a)
