#!/usr/bin/env ruby

instances = []

paths = %w(
  library/Strive/Internal/Options.hs
  library/Strive/Client.hs
) + Dir.glob('library/Strive/Options/*.hs') + Dir.glob('library/Strive/Types/*.hs')

paths.each do |path|
  type = nil

  File.open(path).each_line do |line|
    match = /^data (\w+) =/.match(line)
    type = match[1] if match

    match = /[{,] (\w+) +:: (.+)$/.match(line)
    next unless match

    field = match[1]
    field_type = match[2]
    field_type = "(#{field_type})" if field_type[' ']
    function = field.split('_').last
    klass = "#{function}Lens"
    klass[0] = klass[0].upcase
    function = "#{function}_" if %w(data error id max min type).include?(function)

    instances << <<-HASKELL

instance #{klass} #{type} #{field_type} where
  #{function} f x = fmap
    (\\ y -> x { #{field} = y })
    (f (#{field} x))
    HASKELL
  end
end

puts <<-HASKELL
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Strive.Lenses.Instances where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Client.Conduit (Manager)
import Strive.Client
import Strive.Enums
import Strive.Internal.Options
import Strive.Lenses.Classes
import Strive.Options
import Strive.Types
HASKELL

instances.sort.each do |instance|
  puts instance
end
