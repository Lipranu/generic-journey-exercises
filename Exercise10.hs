{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Exercise10 where

import           GHC.Generics hiding ( conName )
import qualified GHC.Generics as Generics

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving Show

instance Generic (Tree a) where
  type Rep (Tree a) = D1 ('MetaData "Tree" "Shared" "-" 'False)
                    ( C1 ('MetaCons "Leaf" PrefixI 'False) (Rec0 a)
                  :+: C1 ('MetaCons "Node" PrefixI 'False)
                      (Rec0 (Tree a) :*: Rec0 (Tree a)))

  from (Leaf x)   = M1 . L1 . M1 $ K1 x
  from (Node l r) = M1 . R1 . M1 $ K1 l :*: K1 r

  to (M1 (L1 (M1 (K1 x))))          = Leaf x
  to (M1 (R1 (M1 (K1 l :*: K1 r)))) = Node l r

instance Generic1 Tree where
  type Rep1 Tree = D1 ('MetaData "Tree" "Shared" "-" 'False)
                    ( C1 ('MetaCons "Leaf" PrefixI 'False) Par1
                  :+: C1 ('MetaCons "Node" PrefixI 'False)
                      (Rec1 Tree :*: Rec1 Tree))

  from1 (Leaf x)   = M1 . L1 . M1 $ Par1 x
  from1 (Node l r) = M1 . R1 . M1 $ Rec1 l :*: Rec1 r

  to1 (M1 (L1 (M1 (Par1 x))))          = Leaf x
  to1 (M1 (R1 (M1 (Rec1 l :*: Rec1 r)))) = Node l r

instance Eq a => Eq (Tree a) where
  (==) = eq

instance Functor Tree where
  fmap f = gfmap f

data Colour
  = Red
  | Green
  | Blue
  deriving Show

instance Generic Colour where
  type Rep Colour = D1 ('MetaData "Colour" "Shared" "-" 'False)
                  ( C1 ('MetaCons "Red"   PrefixI 'False) U1
                :+: C1 ('MetaCons "Green" PrefixI 'False) U1
                :+: C1 ('MetaCons "Blue"  PrefixI 'False) U1)

  from Red   = M1 . L1 $ M1 U1
  from Green = M1 . R1 . L1 $ M1 U1
  from Blue  = M1 . R1 . R1 $ M1 U1

  to (M1 (L1 (M1 U1)))      = Red
  to (M1 (R1 (L1 (M1 U1)))) = Green
  to (M1 (R1 (R1 (M1 U1)))) = Blue

instance Eq Colour where
  (==) = eq

class GEq f where
  geq :: f a -> f a -> Bool

instance GEq f => GEq (M1 i c f) where
  geq (M1 x) (M1 y) = geq x y

instance (GEq f, GEq g) => GEq (f :+: g) where
  geq (L1 x) (L1 y) = geq x y
  geq (R1 x) (R1 y) = geq x y
  geq _ _ = False

instance (GEq f, GEq g) => GEq (f :*: g) where
  geq (x1 :*: y1) (x2 :*: y2) = geq x1 x2 && geq y1 y2

instance Eq a => GEq (K1 i a) where
  geq (K1 x) (K1 y) = x == y

instance GEq U1 where
  geq _ _ = True

eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
eq x y = geq (from x) (from y)

class GEnum f where
  genum :: [f p]

instance GEnum f => GEnum (M1 i c f) where
  genum = M1 <$> genum

instance (GEnum f, GEnum g) => GEnum (f :+: g) where
  genum = (L1 <$> genum) ++ (R1 <$> genum)

instance GEnum U1 where
  genum = [U1]

enum :: (Generic a, GEnum (Rep a)) => [a]
enum = to <$> genum

class GConName f where
  gConName :: f p -> String

instance (GConName f, GConName g) => GConName (f :+: g) where
  gConName (L1 x) = gConName x
  gConName (R1 x) = gConName x

instance GConName f => GConName (D1 c f) where
  gConName (M1 x) = gConName x

instance Constructor c => GConName (C1 c f) where
  gConName x = Generics.conName x

conName :: (Generic a, GConName (Rep a)) => a -> String
conName = gConName . from

class GConIx f where
  gConIx    :: f p -> Int
  gConCount :: Int

instance GConIx f => GConIx (D1 c f) where
  gConIx (M1 x) = gConIx x
  gConCount     = gConCount @f

instance (GConIx f, GConIx g) => GConIx (f :+: g) where
  gConIx (L1 x) = gConIx x
  gConIx (R1 x) = gConCount @f + gConIx x

  gConCount = gConCount @f + gConCount @g

instance GConIx (C1 c f) where
  gConIx _  = 0
  gConCount = 1

gfmap :: (Generic1 f, Functor (Rep1 f)) => (a -> b) -> f a -> f b
gfmap f = to1 . fmap f . from1

conIx :: (Generic a, GConIx (Rep a)) => a -> Int
conIx = gConIx . from

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
                <> show (x == y)

testEnum :: IO ()
testEnum = putStrLn $ "enum test: Colour: " <> show (enum @Colour)

testConName :: IO ()
testConName = do
  test tree1
  test tree2
  test Red
  test Green
  test Blue
  where test x = putStrLn $ "conName test: " <> show x <> ": " <> conName x

testConIx :: IO ()
testConIx = do
  test tree1
  test tree2
  test Red
  test Green
  test Blue
  putStrLn $ "conIx test: Colours: " <> show (conIx <$> enum @Colour)
  where test x = putStrLn
               $ "conIx test: " <> show x <> ": " <> show (conIx x)

testFmap :: IO ()
testFmap = do
  test tree1
  test tree2
  where test x = putStrLn
               $ "gfmap test: gfmap (+1) ("
              <> show x
              <> "): "
              <> show (fmap (+1) x)

testEx10 :: IO ()
testEx10 = do
  testEq
  testEnum
  testConName
  testConIx
  testFmap
