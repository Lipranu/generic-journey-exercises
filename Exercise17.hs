{-# LANGUAGE GADTs      #-}
{-# LANGUAGE Rank2Types #-}

module Exercise17 where

import Chapter2_10

mapProduct :: SList c xs
           -> (forall c x . f x -> g x)
           -> Product f xs
           -> Product g xs
mapProduct (SCons rs) op (Cons x xs) = Cons (op x) $ mapProduct rs op xs
mapProduct SNil _ Nil = Nil
