module Abt.View
( View(..)
) where

import Data.List

data View v o φ
  = V v
  | Abs v φ
  | App o [φ]

instance functorView :: Functor (View v o) where
  (<$>) f m =
    case m of
      V v -> V v
      Abs v e -> Abs v (f e)
      App o es -> App o (f <$> es)
