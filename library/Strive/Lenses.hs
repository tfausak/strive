{-# LANGUAGE RankNTypes #-}

-- | Lenses for easily getting and setting values.
module Strive.Lenses where

import Data.Functor.Constant (Constant (Constant), getConstant)
import Data.Functor.Identity (Identity (Identity), runIdentity)

-- | A lens for a record.
type Lens a b = Functor f => (b -> f b) -> a -> f a

-- | Get a field from a record.
get :: Lens a b -> a -> b
get lens = getConstant . lens Constant

-- | Update a field in a record
update :: Lens a b -> (b -> b) -> a -> a
update lens f = runIdentity . lens (Identity . f)

-- | Set a field in a record.
set :: Lens a b -> b -> a -> a
set lens x = update lens (const x)
