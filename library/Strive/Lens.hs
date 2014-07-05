{-# LANGUAGE FunctionalDependencies #-}

-- | Lenses for easily getting and setting values.
module Strive.Lens where

-- | A lens for a record, returning a field and a residue.
type Lens a b = a -> (b, b -> a)

-- | Get a field from a record using a lens.
get :: Lens a b -> a -> b
get = (fst .)

-- | Set a field in a record using a lens.
set :: Lens a b -> b -> a -> a
set = flip . (snd .)

-- * Classes

-- TODO

class ApprovalPromptLens a b | a -> b where
  approvalPrompt :: Lens a b

class PrivateScopeLens a b | a -> b where
  privateScope :: Lens a b

class StateLens a b | a -> b where
  state :: Lens a b

class WriteScopeLens a b | a -> b where
  writeScope :: Lens a b
