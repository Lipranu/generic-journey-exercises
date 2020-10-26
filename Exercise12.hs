{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Exercise12 where

import Chapter2_3
import Control.Exception ( PatternMatchFail, catch )
import GHC.TypeLits      ( symbolVal )

data Fail = Fail Int deriving Show

instance Generic Fail where
  type Code Fail = Atom Int

  from (Fail x) = x
  to x = Fail x

gConName :: SRepresentation Top a -> Interpret a -> String
gConName (SSum         l _) (Left  x) = gConName l x
gConName (SSum         _ r) (Right x) = gConName r x
gConName (SConstructor n _) x         = symbolVal n

conName :: forall a . (Generic a, IsRepresentation Top (Code a))
        => a
        -> String
conName = gConName (representation @Top @(Code a)) . from

gConIx :: SRepresentation Top a -> Interpret a -> Int
gConIx (SSum         l _) (Left  x) = gConIx l x
gConIx (SSum         l r) (Right x) = gConCount l + gConIx r x
gConIx (SConstructor _ _) _         = 0

gConCount :: SRepresentation Top a -> Int
gConCount (SSum         l r) = gConCount r + gConCount l
gConCount (SConstructor _ _) = 1

conIx :: forall a . (Generic a, IsRepresentation Top (Code a)) => a -> Int
conIx = gConIx (representation @Top @(Code a)) . from

tree1, tree2 :: Tree Int
tree1 = Node (Leaf 1) (Leaf 2)
tree2 = Leaf 1

testConName :: IO ()
testConName = do
  test Red
  test Blue
  test Green
  test tree2
  test tree1
  (test $ Fail 1) `catch` handler
  where handler e = print (e :: PatternMatchFail)
        test    x = putStrLn
                  $ "conName test: " <> show x <> ": " <> show (conName x)

testConIx :: IO ()
testConIx = do
  putStrLn $ "conIx test: Colours: " <> show (conIx <$> enum @Colour)
  test Red
  test Blue
  test Green
  test tree2
  test tree1
  (test $ Fail 1) `catch` handler
  where handler e = print (e :: PatternMatchFail)
        test    x = putStrLn
                  $ "conIx test: " <> show x <> ": " <> show (conIx x)

testEx12 :: IO ()
testEx12 = testConName >> testConIx
