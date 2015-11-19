-- Thanks /u/AndrasKovacs for pointing out the indexed type families solution
-- in typing ASt:
--   https://goo.gl/77Hk6F
--   https://goo.gl/cQECzb

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Control.IxFix where

import Control.Monad

--    # Helper Types and Functions
--   ## Auxuiliaries
infixr 5 ~>
type f ~> g = forall i. f i -> g i

newtype K a b = K { unK :: a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype I a = I { unI :: a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative I where
  pure = I
  I f <*> I a = I (f a)

instance Monoid a => Applicative (K a) where
  pure _ = K mempty
  K a <*> K a' = K (mappend a a')

--   ## Indexed Type Classes
class IxFunctor (f :: (k -> *) -> (k -> *)) where
  imap :: (a ~> b) -> (f a ~> f b)

imapDefault :: IxTraversable t => (a ~> b) -> (t a ~> t b)
imapDefault f = unI . itraverse (I . f)

-- TODO: use Template Haskell to automatically derive IxTraversable, with:
--   * All non-indexed recursion should also be Traversable.
--   * All indexed recursion should also be IxTraversable.
class IxFunctor t => IxTraversable t where
  itraverse :: Applicative f
            => (forall i. a i -> f (b i))
            -> (forall i. t a i -> f (t b i))

--   ## Indexed Fixed-point Combinator
newtype IxFix f i = In { out :: f (IxFix f) i }

--    # Recursion Schemes
--   ## Catamorphisms
cata :: IxFunctor f => (f a ~> a) -> (IxFix f ~> a)
cata phi = phi . imap (cata phi) . out

cataM :: (Monad m, IxTraversable t)
      => (forall i. t a i -> m (a i))
      -> (forall i. IxFix t i -> m (a i))
cataM phi = phi <=< itraverse (cataM phi) . out

--   ## Anamorphisms
ana :: IxFunctor f => (a ~> f a) -> (a ~> IxFix f)
ana psi = In . imap (ana psi) . psi

anaM :: (Monad m, IxTraversable t)
     => (forall i. a i -> m (t a i))
     -> (forall i. a i -> m (IxFix t i))
anaM psi = fmap In . (itraverse (anaM psi) <=< psi)
