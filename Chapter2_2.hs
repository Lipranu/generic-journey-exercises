{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Chapter2_2 where

import Data.Kind    ( Constraint, Type )
import Data.Proxy   ( Proxy (..) )
import GHC.TypeLits ( KnownSymbol, Symbol )

data Representation
  = Sum         Representation Representation
  | Constructor Symbol         Representation
  | Product     Representation Representation
  | Unit
  | Atom        Type

type family Interpret (r :: Representation) :: Type where
  Interpret (Sum l r)         = Either (Interpret l) (Interpret r)
  Interpret (Constructor n x) = Interpret x
  Interpret (Product l r)     = (Interpret l, Interpret r)
  Interpret Unit              = ()
  Interpret (Atom x)          = x

type Rep a = Interpret (Code a)

class Generic (a :: Type) where
  type Code (a :: Type) :: Representation

  from :: a -> Rep a
  to   :: Rep a -> a

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving Show

instance Generic (Tree a) where
  type Code (Tree a) = Sum (Constructor "Leaf" (Atom a))
                           (Constructor "Node" (Product (Atom (Tree a))
                                                        (Atom (Tree a))))
  from (Leaf x)   = Left x
  from (Node l r) = Right (l, r)

  to (Left x)       = Leaf x
  to (Right (l, r)) = Node l r

instance Eq a => Eq (Tree a) where
  (==) = eq

data Colour
  = Red
  | Green
  | Blue
  deriving Show

instance Generic Colour where
  type Code Colour = Sum (Constructor "Red" Unit)
                         (Sum (Constructor "Green" Unit)
                              (Constructor "Blue" Unit))

  from Red   = Left ()
  from Green = Right $ Left  ()
  from Blue  = Right $ Right ()

  to (Left ())          = Red
  to (Right (Left  ())) = Green
  to (Right (Right ())) = Blue

instance Eq Colour where
  (==) = eq

data SRepresentation (c :: Type -> Constraint) (r :: Representation) where
  SSum         :: SRepresentation c l
               -> SRepresentation c r
               -> SRepresentation c (Sum l r)

  SConstructor :: KnownSymbol n
               => Proxy n
               -> SRepresentation c a
               -> SRepresentation c (Constructor n a)

  SProduct     :: SRepresentation c l
               -> SRepresentation c r
               -> SRepresentation c (Product l r)

  SUnit        :: SRepresentation c Unit

  SAtom        :: c a
               => SRepresentation c (Atom a)

class IsRepresentation (c :: Type -> Constraint) (r :: Representation) where
  representation :: SRepresentation c r

instance (IsRepresentation c l, IsRepresentation c r)
  => IsRepresentation c (Sum l r) where
  representation = SSum representation representation

instance (IsRepresentation c a, KnownSymbol n)
  => IsRepresentation c (Constructor n a) where
  representation = SConstructor Proxy representation

instance (IsRepresentation c l, IsRepresentation c r)
  => IsRepresentation c (Product l r) where
  representation = SProduct representation representation

instance IsRepresentation c Unit where
  representation = SUnit

instance c a => IsRepresentation c (Atom a) where
  representation = SAtom

geq :: SRepresentation Eq a -> Interpret a -> Interpret a -> Bool
geq (SSum         l _) (Left  x) (Left  y) = geq l x y
geq (SSum         _ r) (Right x) (Right y) = geq r x y
geq (SSum         _ _) _         _         = False
geq (SConstructor _ a) x         y         = geq a x y
geq (SProduct     l r) (x1, y1)  (x2, y2)  = geq l x1 x2 && geq r y1 y2
geq SUnit              ()        ()        = True
geq SAtom              x         y         = x == y

eq :: forall a . (Generic a, IsRepresentation Eq (Code a)) => a -> a -> Bool
eq x y = geq (representation @Eq @(Code a)) (from x) (from y)

testCh2_2 :: IO ()
testCh2_2 = do
  test tree1 tree2
  test tree1 tree1
  test tree2 tree2
  test Red   Blue
  test Green Blue
  test Green Red
  test Blue  Blue
  where tree1 = Node (Leaf 1) (Leaf 2)
        tree2 = Leaf 1
        test x y = putStrLn $ "eq test: "
                <> show x <> " == " <> show y <> ": " <> show (x == y)
