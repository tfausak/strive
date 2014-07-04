{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- | Lenses for easily getting and setting values.
module Strive.Lenses where

import Strive.Objects

-- | A lens for a record, returning a field and a residual.
type Lens a b = a -> (b, b -> a)

-- | Get a field from a record using a lens.
get :: Lens a b -> a -> b
get = (fst .)

-- | Set a field in a record using a lens.
set :: Lens a b -> b -> a -> a
set = flip . (snd .)

class IdLens a b | a -> b where
  id :: Lens a b

instance IdLens AthleteMeta Integer where
  id athlete =
    ( athleteMetaId athlete
    , \ id' -> athlete { athleteMetaId = id' }
    )
