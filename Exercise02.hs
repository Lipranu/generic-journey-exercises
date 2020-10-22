{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Exercise02 where

import Shared
import Exercise01

class GCmp a where
  gcmp :: a -> a -> Ordering

instance Ord a => GCmp (Wrap a) where
  gcmp (Wrap x) (Wrap y) = compare x y

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

instance Ord Term where
  compare = cmp

cmp :: (Generic a, GCmp (Rep a)) => a -> a -> Ordering
cmp x y = gcmp (from x) (from y)
