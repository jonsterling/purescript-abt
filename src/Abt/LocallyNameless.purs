module Abt.LocallyNameless
( Abt()
, AbtEffect()
, into
, out
, subst
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

data Abt o
  = Free Name
  | Bound Number
  | Abs Name (Abt o)
  | App o [Abt o]

type AbtEffect e = (err :: Exception, nominal :: NOMINAL | e)

instance eqAbt :: (Eq o) => Eq (Abt o) where
  (==) (Free v) (Free v') = v == v'
  (==) (Bound i) (Bound j) = i == j
  (==) (Abs _ e) (Abs _ e') = e == e'
  (==) (App o es) (App o' es') = o == o' && es == es'
  (/=) m n = not (m == n)

shiftVar :: forall o. Name -> Number -> Abt o -> Abt o
shiftVar v n e =
  case e of
    Free v' -> if v == v' then Bound n else Free v'
    Bound m -> Bound m
    Abs x e -> Abs x $ shiftVar v (n + 1) e
    App o es -> App o $ shiftVar v n <$> es

addVar :: forall o. Name -> Number -> Abt o -> Abt o
addVar v n e =
  case e of
    Free v' -> Free v'
    Bound m -> if m == n then Free v else Bound m
    Abs x e -> Abs x $ addVar v (n + 1) e
    App o es -> App o $ addVar v n <$> es

matchArity :: forall o. Number -> Abt o -> Boolean
matchArity 0 (Abs _ _) = false
matchArity 0 _ = true
matchArity n (Abs _ e') = matchArity (n - 1) e'
matchArity n _ = false

app :: forall o e. (Operator o) => o -> [Abt o] -> Eff (AbtEffect e) (Abt o)
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

into :: forall o e. (Operator o) => V.View Name o (Abt o) -> Eff (AbtEffect e) (Abt o)
into (V.V v) = return $ Free v
into (V.Abs v e) = return $ Abs v (shiftVar v 0 e)
into (V.App o es) = app o es

out :: forall o e. Abt o -> Eff (AbtEffect e) (V.View Name o (Abt o))
out (Free v) = return $ V.V v
out (Bound n) = throwException $ error "Bound variable occured in out"
out (Abs x e) = do
  v <- clone x
  return $ V.Abs v $ addVar v 0 e
out (App o es) = return $ V.App o es

viewIso :: forall o e. (Operator o) => V.ViewIso (Abt o) (Eff (AbtEffect e)) Name o
viewIso = { out : out, into : into }

subst :: forall v o e. (Operator o) => Abt o -> Name -> Abt o -> Eff (AbtEffect e) (Abt o)
subst = V.genericSubst viewIso

printAbt :: forall v o e. (Operator o) => Abt o -> Eff (AbtEffect e) String
printAbt = V.genericPrint viewIso
