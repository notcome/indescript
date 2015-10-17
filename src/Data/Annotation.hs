{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Data.Annotation where

-- I asked the question on #Haskell IRC channel.
-- @glguy kindly provided the following code before I finished reading
-- the first part of the GHC.Generics tutorial on HaskellWiki.
-- Thanks @glguy and @KaneTW for their advices in the IRC channel.
--
-- The code was posted on: http://lpaste.net/143179.

import GHC.Generics

class Annotation f where
  annotation :: f a -> a

genericAnnotation :: (GAnnotation (Rep1 f), Generic1 f) => f a -> a
genericAnnotation = gannotation . from1

class GAnnotation f where
  gannotation :: f a -> a

instance GAnnotation f => GAnnotation (M1 i c f) where
  gannotation (M1 x) = gannotation x

instance GAnnotation g => GAnnotation (f :*: g) where
  gannotation (_ :*: x) = gannotation x

instance (GAnnotation f, GAnnotation g) => GAnnotation (f :+: g) where
  gannotation (L1 x) = gannotation x
  gannotation (R1 x) = gannotation x

instance GAnnotation Par1 where
  gannotation (Par1 x) = x
