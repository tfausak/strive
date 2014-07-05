{-# LANGUAGE FunctionalDependencies #-}

module Strive.Lens where

type Lens a b = a -> (b, b -> a)

get :: Lens a b -> a -> b
get = (fst .)

set :: Lens a b -> b -> a -> a
set = flip . (snd .)

-- TODO: Everything below here should be generated with metaprogramming.

class ApprovalPromptLens a b | a -> b where
  approvalPrompt :: Lens a b

class PrivateScopeLens a b | a -> b where
  privateScope :: Lens a b

class StateLens a b | a -> b where
  state :: Lens a b

class WriteScopeLens a b | a -> b where
  writeScope :: Lens a b
