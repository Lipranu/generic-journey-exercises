{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Exercise05 where

import Chapter1

data Foo
  = Bar Int Int Int
  | Baz Int Int
  deriving Show

instance Generic Foo where
  type Rep Foo = Either (Wrap Int, (Wrap Int, Wrap Int)) (Wrap Int, Wrap Int)

  from (Bar x y z) = Left (Wrap x, (Wrap y, Wrap z))
  from (Baz x y)   = Right (Wrap x, Wrap y)

  to (Left (Wrap x, (Wrap y, Wrap z))) = Bar x y z
  to (Right (Wrap x, Wrap y))          = Baz x y

class GTotal a where
  gtotal :: a -> Int

instance (GTotal a, GTotal b) => GTotal (Either a b) where
  gtotal (Right x) = gtotal x
  gtotal (Left  x) = gtotal x

instance (GTotal a, GTotal b) => GTotal (a, b) where
  gtotal (x, y) = gtotal x + gtotal y

instance GTotal (Wrap Int) where
  gtotal (Wrap x) = x

total :: (Generic a, GTotal (Rep a)) => a -> Int
total = gtotal . from

testEx05 :: IO ()
testEx05 = test bar >> test baz
  where test x = putStrLn $ "total " <> show x <> " = " <> show (total x)
        bar    = Bar 3 4 5
        baz    = Baz 2 8
