{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Shared where

import Data.Kind    ( Type )
import Data.Proxy   ( Proxy (..) )
import GHC.TypeLits ( Symbol, KnownSymbol, symbolVal )

class Generic a where
  type Rep a :: Type
  from :: a     -> Rep a
  to   :: Rep a -> a

newtype Wrap a = Wrap a

newtype Con (n :: Symbol) a = Con a

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving Show

instance Generic (Tree a) where
  type Rep (Tree a) = Either (Con "Leaf" (Wrap a))
                             (Con "Node" (Wrap (Tree a), Wrap (Tree a)))

  from (Leaf x)   = Left  . Con $ Wrap x
  from (Node l r) = Right $ Con (Wrap l, Wrap r)

  to (Left  (Con (Wrap x)))         = Leaf x
  to (Right (Con (Wrap l, Wrap r))) = Node l r

instance Eq a => Eq (Tree a) where
  (==) = eq

data Colour
  = Red
  | Green
  | Blue
  deriving Show

instance Generic Colour where
  type Rep Colour = Either (Con "Red" ())
                           (Either (Con "Green" ())
                                   (Con "Blue" ()))

  from Red   = Left $ Con ()
  from Green = Right . Left  $ Con ()
  from Blue  = Right . Right $ Con ()

  to (Left  (Con ()))         = Red
  to (Right (Left  (Con ()))) = Green
  to (Right (Right (Con ()))) = Blue

instance Eq Colour where
  (==) = eq

class GEq a where
  geq :: a -> a -> Bool

instance GEq a => GEq (Con n a) where
  geq (Con x) (Con y) = geq x y

instance Eq a => GEq (Wrap a) where
  geq (Wrap x) (Wrap y) = x == y

instance (GEq a, GEq b) => GEq (Either a b) where
  geq (Left x)  (Left y)  = geq x y
  geq (Right x) (Right y) = geq x y
  geq _ _ = False

instance (GEq a, GEq b) => GEq (a, b) where
  geq (x1, y1) (x2, y2) = geq x1 x2 && geq y1 y2

instance GEq () where
  geq () () = True

class GEnum a where
  genum :: [a]

instance GEnum () where
  genum = [()]

instance GEnum a => GEnum (Con n a) where
  genum = Con <$> genum

instance (GEnum a, GEnum b) => GEnum (Either a b) where
  genum = (Left <$> genum) ++ (Right <$> genum)

class GConName a where
  gConName :: a -> String

instance (GConName a, GConName b) => GConName (Either a b) where
  gConName (Left  x) = gConName x
  gConName (Right x) = gConName x

instance KnownSymbol n => GConName (Con n a) where
  gConName _ = symbolVal $ Proxy @n

class GConIx a where
  gConIx    :: a -> Int
  gConCount :: Int

instance GConIx (Con n a) where
  gConIx _  = 0
  gConCount = 1

instance (GConIx a, GConIx b) => GConIx (Either a b) where
  gConIx (Left  x) = gConIx x
  gConIx (Right x) = gConCount @a + gConIx x

  gConCount = gConCount @a + gConCount @b

eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
eq x y = geq (from x) (from y)

enum :: (Generic a, GEnum (Rep a)) => [a]
enum = to <$> genum

conName :: (Generic a, GConName (Rep a)) => a -> String
conName = gConName . from

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

testModule1 :: IO ()
testModule1 = do
  testEq
  testEnum
  testConName
  testConIx
