{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Chapter2_1 where

import GHC.TypeLits ( Symbol )
import Data.Kind    ( Type )

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

testEq :: IO ()
testEq = do
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

testEnum :: IO ()
testEnum = putStrLn $ "enum test: Colour: " <> show (enum @Colour)

testCh2_1 :: IO ()
testCh2_1 = testEq >> testEnum
