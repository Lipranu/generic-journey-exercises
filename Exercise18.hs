{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Exercise18 where

import Chapter2_13
import Data.Coerce        ( coerce )
import Data.Functor.Const ( Const (..) )
import Data.Proxy         ( Proxy (..) )

collapseSum :: SList c xs -> Sum (Const a) xs -> a
collapseSum (SCons r) (Suc xs) = collapseSum r xs
collapseSum (SCons _) (Zero x) = coerce x

conName :: forall c a . (HasDatatypeInfo a, IsList c (Code a)) => a -> String
conName = collapseSum ls
        . select ls const (conNames $ Proxy @a)
        . from
  where ls = list @_ @c

testEx18 :: IO ()
testEx18 = do
  test Red
  test Green
  test Blue
  test $ Leaf 1
  test $ Node (Leaf 1) (Leaf 2)
  where test x = putStrLn
               $ "conName test: " <> show x <> ": " <> conName @Top x
