{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Chapter2_7'8 where

import Data.Void ( Void )
import Data.Kind ( Constraint, Type )

type SumRepresentation = [ProductRepresentation]
type ProductRepresentation = [Type]

type family InterpretSum (r :: SumRepresentation) :: Type where
  InterpretSum (x : xs) = Either (InterpretProduct x) (InterpretSum xs)
  InterpretSum '[] = Void

type family InterpretProduct (r :: ProductRepresentation) :: Type where
  InterpretProduct (x : xs) = (x, InterpretProduct xs)
  InterpretProduct '[] = ()

type Rep a = Interpret (Code a)
type Representation = SumRepresentation
type Interpret a = InterpretSum a

class Generic (a :: Type) where
  type Code (a :: Type) :: Representation

  from :: a -> Rep a
  to   :: Rep a -> a

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving Show

instance Generic (Tree a) where
  type Code (Tree a) = '[ '[a], '[Tree a, Tree a]]

  from (Leaf   x) = Left (x, ())
  from (Node l r) = Right $ Left (l, (r, ()))

  to (Left (x, ()))              = Leaf x
  to (Right (Left (l, (r, ())))) = Node l r

instance Eq a => Eq (Tree a) where
  (==) = eq

data Colour
  = Red
  | Green
  | Blue
  deriving Show

instance Generic Colour where
  type Code Colour = '[ '[], '[], '[] ]

  from Red   = Left ()
  from Green = Right $ Left ()
  from Blue  = Right $ Right $ Left ()

  to (Left ())                 = Red
  to (Right (Left ()))         = Green
  to (Right (Right (Left ()))) = Blue

instance Eq Colour where
  (==) = eq

data SList (c :: a -> Constraint) (xs :: [a]) where
  SCons :: c x => SList c xs -> SList c (x : xs)
  SNil  :: SList c '[]

class IsList (c :: a -> Constraint) (xs :: [a]) where
  list :: SList c xs

instance (c x, IsList c xs) => IsList c (x : xs) where
  list = SCons list

instance IsList c '[] where
  list = SNil

type IsRepresentation c xss = IsList (IsList c) xss

--geqProduct :: SList Eq x -> InterpretProduct x -> InterpretProduct x -> Bool
--geqProduct (SCons rs) (x, xs) (y, ys) = x == y && geqProduct rs xs ys
--geqProduct SNil       ()      ()      = True

zipProduct :: SList c x
           -> (forall x . c x => x -> x -> r)
           -> InterpretProduct x
           -> InterpretProduct x
           -> [r]
zipProduct (SCons rs) op (x, xs) (y, ys) = x `op` y : zipProduct rs op xs ys
zipProduct SNil       _  ()      ()      = []

geqSum :: SList (IsList Eq) a -> InterpretSum a -> InterpretSum a -> Bool
geqSum xs@(SCons _) (Left x) (Left y) = go xs x y
  where go :: forall xs xss . SList (IsList Eq) (xs : xss)
           -> InterpretProduct xs
           -> InterpretProduct xs
           -> Bool
        go (SCons _) = geqProduct (list @_ @Eq @xs)
geqSum (SCons rs) (Right x) (Right y) = geqSum rs x y
geqSum (SCons  _) _         _         = False

geqProduct :: SList Eq a -> InterpretProduct a -> InterpretProduct a -> Bool
geqProduct xs x y = and $ zipProduct xs (==) x y

eq :: forall a . (Generic a, IsRepresentation Eq (Code a)) => a -> a -> Bool
eq x y = geqSum (list @_ @(IsList Eq) @(Code a)) (from x) (from y)

testCh2_7'8 :: IO ()
testCh2_7'8 = do
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
