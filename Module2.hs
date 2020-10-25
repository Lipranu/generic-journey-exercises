{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Module2 where

import Control.Exception ( PatternMatchFail, catch )
import Data.Kind         ( Constraint, Type )
import Data.Proxy        ( Proxy (..) )
import GHC.TypeLits      ( KnownSymbol, Symbol )

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

class GEq (r :: Representation) where
  geq :: Interpret r -> Interpret r -> Bool

instance (GEq l, GEq r) => GEq (Sum l r) where
  geq (Left  x) (Left  y) = geq @l x y
  geq (Right x) (Right y) = geq @r x y
  geq _         _         = False

instance GEq a => GEq (Constructor n a) where
  geq = geq @a

instance (GEq l, GEq r) => GEq (Product l r) where
  geq (x1, y1) (x2, y2) = geq @l x1 x2 && geq @r y1 y2

instance Eq a => GEq (Atom a) where
  geq = (==)

instance GEq Unit where
  geq () () = True

eq :: forall a . (Generic a, GEq (Code a)) => a -> a -> Bool
eq x y = geq @(Code a) (from x) (from y)

class GEnum (r :: Representation) where
  genum :: [Interpret r]

instance (GEnum l, GEnum r) => GEnum (Sum l r) where
  genum = (Left <$> genum @l) ++ (Right <$> genum @r)

instance GEnum a => GEnum (Constructor n a) where
  genum = genum @a

instance GEnum Unit where
  genum = [()]

enum :: forall a . (Generic a, GEnum (Code a)) => [a]
enum = to <$> genum @(Code a)

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

class Top a
instance Top a

class (c a, d a) => And c d a
instance (c a, d a) => And c d a

geq' :: SRepresentation Eq a -> Interpret a -> Interpret a -> Bool
geq' (SSum         l _) (Left  x) (Left  y) = geq' l x y
geq' (SSum         _ r) (Right x) (Right y) = geq' r x y
geq' (SSum         _ _) _         _         = False
geq' (SConstructor _ a) x         y         = geq' a x y
geq' (SProduct     l r) (x1, y1)  (x2, y2)  = geq' l x1 x2 && geq' r y1 y2
geq' SUnit              ()        ()        = True
geq' SAtom              x         y         = x == y

eq' :: forall a . (Generic a, IsRepresentation Eq (Code a)) => a -> a -> Bool
eq' x y = geq' (representation @Eq @(Code a)) (from x) (from y)

genum' :: SRepresentation Top a -> [Interpret a]
genum' (SSum l r) = (Left <$> genum' l) ++ (Right <$> genum' r)
genum' (SConstructor _ a) = genum' a
genum' SUnit = [()]

enum' :: forall a . (Generic a, IsRepresentation Top (Code a)) => [a]
enum' = to <$> genum' (representation @Top @(Code a))

tree1, tree2 :: Tree Integer
tree1 = Node (Leaf 1) (Leaf 2)
tree2 = Leaf 1

testEq :: IO ()
testEq = do
  test tree1 tree2
  test tree1 tree1
  test tree2 tree2
  test Red   Blue
  test Green Blue
  test Green Red
  test Blue  Blue
  where test x y = putStrLn
                 $ "eq test: "
                <> show x
                <> " == "
                <> show y
                <> ": "
                <> show (eq x y)

testEnum :: IO ()
testEnum = putStrLn $ "enum test: Colour: " <> show (enum @Colour)

testEq' = do
  test tree1 tree2
  test tree1 tree1
  test tree2 tree2
  test Red   Blue
  test Green Blue
  test Green Red
  test Blue  Blue
  where test x y = putStrLn
                 $ "eq' test: "
                <> show x
                <> " == "
                <> show y
                <> ": "
                <> show (eq' x y)

testEnum' :: IO ()
testEnum' = do
  test "Colour: " (show (enum' @Colour))
  (test "Tree: "  (show (enum' @(Tree Int)))) `catch` handler
  where test  n x = putStrLn $ "enum' test: " <> n <> x
        handler e = print (e :: PatternMatchFail)

testModule2 :: IO ()
testModule2 = do
  testEq
  testEnum
  testEq'
  testEnum'
