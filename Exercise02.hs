{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Exercise02 where

import Chapter1

class GCmp a where
  gcmp :: a -> a -> Ordering

instance GCmp a => GCmp (Con n a) where
  gcmp (Con x) (Con y) = gcmp x y

instance Ord a => GCmp (Wrap a) where
  gcmp (Wrap x) (Wrap y) = compare x y

instance GCmp () where
  gcmp () () = EQ

instance (GCmp a, GCmp b) => GCmp (Either a b) where
  gcmp (Right x) (Right y) = gcmp x y
  gcmp (Left  x) (Left  y) = gcmp x y
  gcmp (Left  _) _         = LT
  gcmp (Right _) _         = GT

instance (GCmp a, GCmp b) => GCmp (a, b) where
  gcmp (x1, y1) (x2, y2)  = case gcmp x1 x2 of
    EQ   -> gcmp y1 y2
    rest -> rest

instance Ord a => Ord (Tree a) where
  compare = cmp

instance Ord Colour where
  compare = cmp

cmp :: (Generic a, GCmp (Rep a)) => a -> a -> Ordering
cmp x y = gcmp (from x) (from y)

testEx02 :: IO ()
testEx02 = do
  test tree1 tree2
  test tree1 tree1
  test tree2 tree2
  test Red   Blue
  test Green Blue
  test Green Red
  test Blue  Blue
  where test x y = putStrLn
                 $ "cmp test: "
                <> show x
                <> " `compare` "
                <> show y
                <> ": "
                <> show (compare x y)
