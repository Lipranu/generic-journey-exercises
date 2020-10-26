{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Chapter2_7 where

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
