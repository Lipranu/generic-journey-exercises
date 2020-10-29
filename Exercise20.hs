{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Exercise20 where

import Chapter2_13
import Exercise17

data Projection f xs x = MkProjection (Product f xs -> f x)

projections :: forall c f xs . SList c xs -> Product (Projection f xs) xs
projections (SCons rs) = Cons
  (MkProjection (\(Cons x _) -> x))
  (mapProduct rs shiftProjection (projections @c @f rs))
projections SNil = Nil

shiftProjection :: Projection f xs x -> Projection f (y : xs) x
shiftProjection (MkProjection proj) = MkProjection (\(Cons _ xs) -> proj xs)
