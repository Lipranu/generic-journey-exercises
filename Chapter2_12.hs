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
{-# LANGUAGE FlexibleContexts      #-}

module Chapter2_12 where

import Data.Coerce           ( coerce )
import Data.Functor.Const    ( Const (..) )
import Data.Functor.Identity ( Identity (..) )
import Data.Kind             ( Constraint, Type )

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

instance Eq a => Eq (Tree a) where
  (==) = eq

data Colour
  = Red
  | Green
  | Blue
  deriving Show

instance Generic Colour where
  type Code Colour = '[ '[], '[], '[] ]

  from Red   = Zero Nil
  from Green = Suc $ Zero Nil
  from Blue  = Suc . Suc $ Zero Nil

  to (Zero Nil)             = Red
  to (Suc (Zero Nil))       = Green
  to (Suc (Suc (Zero Nil))) = Blue

instance Eq Colour where
  (==) = eq

zipProduct :: SList c xs
           -> (forall x . c x => f x -> g x -> h x)
           -> Product f xs
           -> Product g xs
           -> Product h xs
zipProduct (SCons xs) op (Cons fx fxs) (Cons gx gxs)
  = Cons (op fx gx) $ zipProduct xs op fxs gxs
zipProduct SNil _ Nil Nil = Nil

collapseProduct :: SList c xs -> Product (Const a) xs -> [a]
collapseProduct (SCons xs) (Cons (Const a) as) = a : collapseProduct xs as
collapseProduct SNil Nil = []

geqProduct :: SList Eq xs
           -> Product Identity xs
           -> Product Identity xs
           -> Bool
geqProduct rs xs ys = and . collapseProduct rs
  $ zipProduct rs (\x y -> coerce (x == y)) xs ys

geqSum :: SList (IsList Eq) a
       -> Sum (Product Identity) a
       -> Sum (Product Identity) a
       -> Bool
geqSum xs@(SCons _) (Zero x) (Zero y) = go xs x y
  where go :: forall xs xss . SList (IsList Eq) (xs : xss)
           -> Product Identity xs
           -> Product Identity xs
           -> Bool
        go (SCons _) = geqProduct (list @_ @Eq @xs)
geqSum (SCons rs) (Suc x) (Suc y) = geqSum rs x y
geqSum (SCons  _) _         _         = False

eq :: forall a . (Generic a, IsList (IsList Eq) (Code a)) => a -> a -> Bool
eq x y = geqSum (list @_ @(IsList Eq)) (from x) (from y)

pureProduct :: SList c xs -> (forall x . c x => f x) -> Product f xs
pureProduct (SCons rs) op = Cons op $ pureProduct rs op
pureProduct SNil       _  = Nil

type IsProductType a xs = (Generic a, Code a ~ '[xs])

gmempty :: forall a xs . (IsProductType a xs, IsList Monoid xs) => a
gmempty = to $ Zero $ pureProduct (list @_ @Monoid) $ Identity mempty

type IsEnumType a = (Generic a, IsList ((~) '[]) (Code a))

genum :: SList ((~) '[]) xs -> [Sum (Product Identity) xs]
genum (SCons xs) = Zero Nil : (Suc <$> genum xs)
genum SNil = []

enum :: IsEnumType a => [a]
enum = to <$> genum (list @_ @((~) '[]))

testEq :: IO ()
testEq = do
  test tree1 tree2
  test tree1 tree1
  test tree2 tree2
  test Red   Blue
  test Green Blue
  test Green Red
  test Blue  Blue
  where tree1    = Node (Leaf 1) (Leaf 2)
        tree2    = Leaf 1
        test x y = putStrLn $ "eq test: "
                <> show x <> " == " <> show y <> ": " <> show (x == y)

testEnum :: IO ()
testEnum = putStrLn $ "enum test: Colour: " <> show (enum @Colour)

testCh2_12 :: IO ()
testCh2_12 = testEq >> testEnum
