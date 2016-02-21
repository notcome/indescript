module Control.SemiIso where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad

data SemiIso a b = SemiIso
  { apply   :: a -> b
  , unapply :: b -> Maybe a
  }

instance Category SemiIso where
  id = SemiIso id (Just . id)
  g . f = SemiIso in' out where
    in' = apply g   <<< apply f
    out = unapply g >=> unapply f

nil :: SemiIso () [a]
nil = SemiIso (const []) (const $ Just ())

cons :: SemiIso (a, [a]) [a]
cons = SemiIso in' out where
  in' (x, xs) = x : xs
  out []      = Nothing
  out (x:xs)  = Just (x, xs)
