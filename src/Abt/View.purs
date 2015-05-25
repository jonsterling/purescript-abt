module Abt.View
( View(..)

, ViewIso(..)
, genericSubst
) where

import Control.Monad
import Data.List
import Data.Traversable

data View v o φ
  = V v
  | Abs v φ
  | App o [φ]

type ViewIso φ m v o = { out :: φ -> m (View v o φ), into :: View v o φ -> m φ }

genericSubst :: forall φ m v o. (Eq v, Monad m) => ViewIso φ m v o -> φ -> v -> φ -> m φ
genericSubst iso e v e' = do
  oe' <- iso.out e'
  case oe' of
    V v' -> return $ if v == v' then e else e'
    Abs v' e'' -> (Abs v' <$> genericSubst iso e v e'') >>= iso.into
    App o es -> (App o <$> genericSubst iso e v `traverse` es) >>= iso.into

instance functorView :: Functor (View v o) where
  (<$>) f m =
    case m of
      V v -> V v
      Abs v e -> Abs v (f e)
      App o es -> App o (f <$> es)
