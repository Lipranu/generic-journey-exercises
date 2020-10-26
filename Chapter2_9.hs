{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter2_9 where

import Data.Kind             ( Constraint, Type )
import Data.Functor.Identity ( Identity (..) )

data Sum (f :: k -> Type) (xs :: [k]) where
  Zero :: f x      -> Sum f (x : xs)
  Suc  :: Sum f xs -> Sum f (x : xs)

data Product (f :: k -> Type) (xs :: [k]) where
  Nil  :: Product f '[]
  Cons :: f x -> Product f xs -> Product f (x : xs)

type Interpret xss = Sum (Product Identity) xss

type Rep a = Interpret (Code a)

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
