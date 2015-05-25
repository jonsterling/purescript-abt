module Abt.Operator where

class (Eq o, Show o) <= Operator o where
  arity :: o -> [Number]
