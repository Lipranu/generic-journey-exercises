{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Exercise11 where

import Module2
import GHC.TypeLits  ( KnownSymbol )

data Foo
  = Bar Int Int Int
  | Baz Int Int
  deriving Show

instance Generic Foo where
  type Code Foo = Sum (Constructor "Bar" (Product (Atom Int)
                                         (Product (Atom Int) (Atom Int))))
                      (Constructor "Baz" (Product (Atom Int) (Atom Int)))

  from (Bar x y z) = Left  (x, (y, z))
  from (Baz x y)   = Right (x, y)

  to (Left  (x, (y, z))) = Bar x y z
  to (Right (x, y))      = Baz x y

class GTotal a where
  gtotal :: Interpret a -> Integer

instance (GTotal l, GTotal r) => GTotal (Sum l r) where
  gtotal (Left  x) = gtotal @l x
  gtotal (Right x) = gtotal @r x

instance (GTotal a, KnownSymbol n) => GTotal (Constructor n a) where
  gtotal x = gtotal @a x

instance (GTotal l, GTotal r) => GTotal (Product l r) where
  gtotal (x, y) = gtotal @l x + gtotal @r y

instance Integral a => GTotal (Atom a) where
  gtotal x = toInteger x

instance GTotal Unit where
  gtotal () = 0

total :: forall a . (Generic a, GTotal (Code a)) => a -> Integer
total = gtotal @(Code a) . from

gtotal' :: SRepresentation Integral a -> Interpret a -> Integer
gtotal' (SSum l _)         (Left  x) = gtotal' l x
gtotal' (SSum _ r)         (Right x) = gtotal' r x
gtotal' (SConstructor _ a) x         = gtotal' a x
gtotal' (SProduct l r)     (x, y)    = gtotal' l x + gtotal' r y
gtotal' SUnit              ()        = 0
gtotal' SAtom              x         = toInteger x

total' :: forall a . (Generic a, IsRepresentation Integral (Code a))
       => a
       -> Integer
total' = gtotal' (representation @Integral @(Code a)) . from

testEx11 :: IO ()
testEx11 = test bar >> test baz >> test' bar >> test' baz
  where test  x = putStrLn $ "total "  <> show x <> " = " <> show (total  x)
        test' x = putStrLn $ "total' " <> show x <> " = " <> show (total' x)
        bar     = Bar 3 4 5
        baz     = Baz 2 8
