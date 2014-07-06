#!/usr/bin/env ruby

instances = []

%w(
  library/Strive/Client.hs
  library/Strive/Options.hs
  library/Strive/Types.hs
).each do |path|
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
    record = type.dup
    record[0] = record[0].downcase

    instances << <<-HASKELL

instance #{klass} #{type} #{field_type} where
  #{function} #{record} =
    ( #{field} #{record}
    , \\ #{function}' -> #{record} { #{field} = #{function}' }
    )
    HASKELL
  end
end

puts <<-HASKELL
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Strive.Lenses.Instances where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Client.Conduit (Manager)
import Strive.Client
import Strive.Lenses.Classes
import Strive.Options
import Strive.Types
HASKELL

instances.sort.each do |instance|
  puts instance
end
