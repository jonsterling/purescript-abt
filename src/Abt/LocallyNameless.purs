module Abt.LocallyNameless
( Tm()
, into
, out
) where

import Abt.Nominal
import Abt.Operator
import qualified Abt.View as V
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import qualified Data.Array as Array
import Data.List
import Data.Foldable
import Data.Tuple

data Tm o
  = Free Name
  | Bound Number
  | Abs Name (Tm o)
  | App o [Tm o]

instance eqTm :: (Eq o) => Eq (Tm o) where
  (==) (Free v) (Free v') = v == v'
  (==) (Bound i) (Bound j) = i == j
  (==) (Abs _ e) (Abs _ e') = e == e'
  (==) (App o es) (App o' es') = o == o' && es == es'
  (/=) m n = not (m == n)

shiftVar :: forall o. Name -> Number -> Tm o -> Tm o
shiftVar v n e =
  case e of
    Free v' -> if v == v' then Bound n else Free v'
    Bound m -> Bound m
    Abs x e -> Abs x $ shiftVar v (n + 1) e
    App o es -> App o $ shiftVar v n <$> es

addVar :: forall o. Name -> Number -> Tm o -> Tm o
addVar v n e =
  case e of
    Free v' -> Free v'
    Bound m -> if m == n then Free v else Bound m
    Abs x e -> Abs x $ addVar v (n + 1) e
    App o es -> App o $ addVar v n <$> es

matchArity :: forall o. Number -> Tm o -> Boolean
matchArity 0 (Abs _ _) = false
matchArity 0 _ = true
matchArity n (Abs _ e') = matchArity (n - 1) e'
matchArity n _ = false

app :: forall o e. (Operator o) => o -> [Tm o] -> Eff (err :: Exception | e) (Tm o)
app o es =
  let
    ar = arity o
  in
    if Array.length ar /= Array.length es
    then throwException $ error "Incorrect number of arguments"
    else
      if foldl (\b (Tuple x y) -> b && matchArity x y) true (zip ar es)
      then return $ App o es
      else throwException $ error "Incorrect valence"

into :: forall v o e. (Operator o) => V.View Name o (Tm o) -> Eff (err :: Exception | e) (Tm o)
into (V.V v) = return $ Free v
into (V.Abs v e) = return $ Abs v (shiftVar v 0 e)
into (V.App o es) = app o es

out :: forall v o e. Tm o -> Eff (err :: Exception, nominal :: NOMINAL | e) (V.View Name o (Tm o))
out (Free v) = return $ V.V v
out (Bound n) = throwException $ error "Bound variable occured in out"
out (Abs x e) = do
  v <- clone x
  return $ V.Abs v $ addVar v 0 e
out (App o es) = return $ V.App o es

