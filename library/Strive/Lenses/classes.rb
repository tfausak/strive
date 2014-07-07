#!/usr/bin/env ruby

classes = {}

%w(
  library/Strive/Client.hs
  library/Strive/Options.hs
  library/Strive/Types.hs
).each do |path|
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

module Strive.Lenses.Classes where

import Strive.Lenses (Lens)
HASKELL

classes.sort.each do |klass, function|
  puts <<-HASKELL

class #{klass} a b | a -> b where
  #{function} :: Lens a b
  HASKELL
end
