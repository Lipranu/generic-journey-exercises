{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Exercise09 where

import Shared
import Data.Kind    ( Type )
import GHC.TypeLits ( Symbol )

class Generic1 (f :: k -> Type) where
  type Rep1 f :: k -> Type

  from1 :: f x -> (Rep1 f) x
  to1   :: (Rep1 f) x -> f x

data (f :+: g) x = L1 (f x) | R1 (g x)
data (f :*: g) x = (f x) :*: (g x)

newtype M1 (n :: Symbol) f x = M1 (f x)
newtype Rec1 f x = Rec1 (f x)
newtype Par1 x = Par1 x

instance Generic1 Tree where
  type Rep1 Tree = (M1 "Leaf" Par1) :+: (M1 "Node" (Rec1 Tree :*: Rec1 Tree))

  from1 (Leaf x)   = L1 . M1 $ Par1 x
  from1 (Node l r) = R1 . M1 $ Rec1 l :*: Rec1 r

  to1 (L1 (M1 (Par1 x))) = Leaf x
  to1 (R1 (M1 (Rec1 l :*: Rec1 r))) = Node l r

instance Functor Tree where
  fmap f = gfmap f

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (L1 x) = L1 $ fmap f x
  fmap f (R1 x) = R1 $ fmap f x

instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap f (x :*: y) = fmap f x :*: fmap f y

instance Functor f => Functor (M1 n f) where
  fmap f (M1 x) = M1 $ fmap f x

instance Functor f => Functor (Rec1 f) where
  fmap f (Rec1 x) = Rec1 $ fmap f x

instance Functor Par1 where
  fmap f (Par1 x) = Par1 $ f x

gfmap :: (Generic1 f, Functor (Rep1 f)) => (a -> b) -> f a -> f b
gfmap f = to1 . fmap f . from1

testEx09 :: IO ()
testEx09 = do
  test tree1
  test tree2
  where test x = putStrLn
               $ "gfmap test: gfmap (+1) ("
              <> show x
              <> "): "
              <> show (fmap (+1) x)
