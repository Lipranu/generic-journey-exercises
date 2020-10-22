{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}

module Shared where

import Data.Kind ( Type )

class Generic a where
  type Rep a :: Type
  from :: a     -> Rep a
  to   :: Rep a -> a

class GEq a where
  geq :: a -> a -> Bool

newtype Wrap a = Wrap a

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving Show

instance Generic (Tree a) where
  type Rep (Tree a) = Either (Wrap a) (Wrap (Tree a), Wrap (Tree a))

  from (Leaf x)   = Left $ Wrap x
  from (Node l r) = Right (Wrap l, Wrap r)

  to (Left (Wrap x))          = Leaf x
  to (Right (Wrap l, Wrap r)) = Node l r

instance Eq a => Eq (Tree a) where
  (==) = eq

instance Eq a => GEq (Wrap a) where
  geq (Wrap x) (Wrap y) = x == y

instance (GEq a, GEq b) => GEq (Either a b) where
  geq (Left x)  (Left y)  = geq x y
  geq (Right x) (Right y) = geq x y
  geq _ _ = False

instance (GEq a, GEq b) => GEq (a, b) where
  geq (x1, y1) (x2, y2) = geq x1 x2 && geq y1 y2

class GEnum a where
  genum :: [a]

data Colour
  = Red
  | Green
  | Blue
  deriving Show

instance Generic Colour where
  type Rep Colour = Either () (Either () ())

  from Red   = Left ()
  from Green = Right $ Left ()
  from Blue  = Right $ Right ()

  to (Left _)          = Red
  to (Right (Left _))  = Green
  to (Right (Right _)) = Blue

instance GEq () where
  geq () () = True

instance Eq Colour where
  (==) = eq

instance GEnum () where
  genum = [()]

instance (GEnum a, GEnum b) => GEnum (Either a b) where
  genum = (Left <$> genum) ++ (Right <$> genum)

eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
eq x y = geq (from x) (from y)

enum :: (Generic a, GEnum (Rep a)) => [a]
enum = to <$> genum

tree1, tree2 :: Tree Integer
tree1 = Node (Leaf 1) (Leaf 2)
tree2 = Leaf 1

eqTest :: (Eq a, Show a) => a -> a -> IO ()
eqTest x y = putStrLn $ show x <> " == " <> show y <> ": " <> show (x == y)

eqTree :: IO ()
eqTree = do
  eqTest tree1 tree2
  eqTest tree1 tree1
  eqTest tree2 tree2
