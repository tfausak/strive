#!/usr/bin/env ruby

classes = {}

paths = %w(
  library/Strive/Internal/Options.hs
  library/Strive/Client.hs
) + Dir.glob('library/Strive/Options/*.hs') + Dir.glob('library/Strive/Types/*.hs')

paths.each do |path|
  File.open(path).each_line do |line|
    match = /[{,] (\w+) +::/.match(line)
    next unless match

    function = match[1].split('_').last
    klass = "#{function}Lens"
    klass[0] = klass[0].upcase
    function = "#{function}_" if %w(data error id max min type).include?(function)

    classes[klass] = function
  end
end

puts <<-HASKELL
{-# LANGUAGE FunctionalDependencies #-}

-- | Automatically generated lens classes.
module Strive.Lenses.Classes
  ( #{classes.keys.sort.map { |klass| "#{klass} (..)" }.join("\n  , ")}
  ) where

import Strive.Lenses (Lens)
HASKELL

classes.sort.each do |klass, function|
  puts <<-HASKELL

-- | Class for the '#{function}' lens.
class #{klass} a b | a -> b where
  #{function} :: Lens a b
  HASKELL
end
