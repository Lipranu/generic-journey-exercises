{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE GADTs    #-}

module Exercise22 where

import Chapter2_13
import Exercise18
import Data.Functor.Const ( Const (..) )

conIx :: forall a . (HasDatatypeInfo a, IsList Top (Code a)) => a -> Int
conIx = collapseSum ls
      . select ls const (gConIx ls 0)
      . from
  where ls = list @_ @Top

gConIx :: SList c xs -> Int -> Product (Const Int) xs
gConIx (SCons rs) i = Const i `Cons` gConIx rs (i + 1)
gConIx SNil       _ = Nil

testEx22 :: IO ()
testEx22 = do
  test Red
  test Green
  test Blue
  test $ Leaf 1
  test $ Node (Leaf 1) (Leaf 2)
  putStrLn $ "conIx test: Colours: " <> show (conIx <$> enum @Colour)
  where test x = putStrLn
               $ "conIx test: " <> show x <> ": " <> show (conIx x)
