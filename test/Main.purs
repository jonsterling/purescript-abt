module Test.Main where

import Debug.Trace

import Abt.View
import Abt.Nominal
import Abt.Operator
import Abt.LocallyNameless

import Control.Monad

data Op = LAM | AP | AX

instance eqOp :: Eq Op where
  (==) LAM LAM = true
  (==) AP AP = true
  (==) AX AX = true
  (==) _ _ = false
  (/=) o1 o2 = not $ o1 == o2

instance showOp :: Show Op where
  show LAM = "λ"
  show AP = "ap"
  show AX = "<>"

instance operatorOp :: Operator Op where
  arity LAM = [1]
  arity AP = [0,0]
  arity AX = []

(=<<) :: forall a b m. (Monad m) => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

main = do
  x <- new "x"
  e <- (x \) =<< var x
  e' <- LAM $$ [e]
  e'' <- printAbt e'
  trace e''
