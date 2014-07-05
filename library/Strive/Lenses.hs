-- | Lenses for easily getting and setting values.
module Strive.Lenses where

-- | A lens for a record, returning a field and a residue.
type Lens a b = a -> (b, b -> a)

-- | Get a field from a record using a lens.
get :: Lens a b -> a -> b
get = (fst .)

-- | Set a field in a record using a lens.
set :: Lens a b -> b -> a -> a
set = flip . (snd .)
