{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE FlexibleContexts      #-}

module Chapter2_11 where

import Data.Coerce           ( coerce )
import Data.Functor.Const    ( Const (..) )
import Data.Functor.Identity ( Identity (..) )
import Data.Kind             ( Constraint, Type )
import qualified Data.Monoid as M

data Sum (f :: k -> Type) (xs :: [k]) where
  Zero :: f x      -> Sum f (x : xs)
  Suc  :: Sum f xs -> Sum f (x : xs)

data Product (f :: k -> Type) (xs :: [k]) where
  Nil  :: Product f '[]
  Cons :: f x -> Product f xs -> Product f (x : xs)

type Interpret xss = Sum (Product Identity) xss

type Rep a = Interpret (Code a)

data SList (c :: a -> Constraint) (xs :: [a]) where
  SCons :: c x => SList c xs -> SList c (x : xs)
  SNil  :: SList c '[]

class IsList (c :: a -> Constraint) (xs :: [a]) where
  list :: SList c xs

instance (c x, IsList c xs) => IsList c (x : xs) where
  list = SCons list

instance IsList c '[] where
  list = SNil

class Generic a where
  type Code a :: [[Type]]

  from :: a -> Rep a
  to   :: Rep a -> a

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving Show

instance Generic (Tree a) where
  type Code (Tree a) = '[ '[a], '[Tree a, Tree a] ]

  from (Leaf x)   = Zero (Identity x `Cons` Nil)
  from (Node l r) = Suc (Zero (Identity l `Cons` (Identity r `Cons` Nil)))

  to (Zero (Identity x `Cons` Nil))                           = Leaf x
  to (Suc (Zero (Identity l `Cons` (Identity r `Cons` Nil)))) = Node l r

data MonoidTest a b c = MonoidTest a b c deriving Show

instance Generic (MonoidTest a b c) where
  type Code (MonoidTest a b c) = '[ '[a, b, c] ]

  from (MonoidTest x y z) = Zero (Identity x `Cons`
                                 (Identity y `Cons`
                                 (Identity z `Cons` Nil)))

  to (Zero (Identity x `Cons` (Identity y `Cons` (Identity z `Cons` Nil))))
    = MonoidTest x y z

zipProduct :: SList c xs
           -> (forall x . c x => f x -> g x -> h x)
           -> Product f xs
           -> Product g xs
           -> Product h xs
zipProduct (SCons xs) op (Cons fx fxs) (Cons gx gxs)
  = Cons (op fx gx) $ zipProduct xs op fxs gxs
zipProduct SNil _ Nil Nil = Nil

collapseProduct :: SList c xs -> Product (Const a) as -> [a]
collapseProduct (SCons xs) (Cons (Const a) as) = a : collapseProduct xs as
collapseProduct SNil Nil = []

geqProduct :: SList Eq xs
           -> Product Identity xs
           -> Product Identity xs
           -> Bool
geqProduct rs xs ys = and . collapseProduct rs
  $ zipProduct rs (\x y -> coerce (x == y)) xs ys

pureProduct :: SList c xs -> (forall x . c x => f x) -> Product f xs
pureProduct (SCons rs) op = Cons op $ pureProduct rs op
pureProduct SNil       _  = Nil

type IsProductType a xs = (Generic a, Code a ~ '[xs])

gmempty :: forall a xs . (IsProductType a xs, IsList Monoid xs) => a
gmempty = to $ Zero $ pureProduct (list @_ @Monoid) $ Identity mempty

m :: MonoidTest (M.Sum Integer) (M.Product Integer) M.Any
m = MonoidTest (M.Sum 1) (M.Product 2) (M.Any True)
