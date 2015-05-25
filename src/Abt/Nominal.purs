module Abt.Nominal
( Name()
, NOMINAL()
, new
, clone
) where

import Control.Monad.Eff

newtype Name = Name { name :: String, index :: Number }

foreign import data NOMINAL :: !
foreign import freshIndex
  """
  function freshIndex() {
    if (freshIndex.prototype.counter === undefined) {
       freshIndex.prototype.counter = 0;
    }

    freshIndex.prototype.counter += 1;
    return freshIndex.prototype.counter;
  }
  """ :: forall e. Eff (nominal :: NOMINAL | e) Number

new :: forall e. String -> Eff (nominal :: NOMINAL | e) Name
new str = do
  idx <- freshIndex
  return $ Name { name : str, index : idx}

clone :: forall e. Name -> Eff (nominal :: NOMINAL | e) Name
clone (Name nm) = do
  idx <- freshIndex
  return $ Name $ nm { index = idx }

instance showName :: Show Name where
  show (Name nm) = nm.name ++ "@" ++ show nm.index

instance eqName :: Eq Name where
  (==) (Name n1) (Name n2) = n1.index == n2.index
  (/=) n1 n2 = not $ n1 == n2
