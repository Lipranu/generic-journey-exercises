{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}

module Exercise17 where

import Chapter2_13 hiding    ( genum )
import Data.Functor.Const    ( Const (..) )
import Data.Functor.Identity ( Identity (..) )

mapProduct :: SList c xs
           -> (forall c x . f x -> g x)
           -> Product f xs
           -> Product g xs
mapProduct (SCons rs) op (Cons x xs) = Cons (op x) $ mapProduct rs op xs
mapProduct SNil _ Nil = Nil

newtype Injection f xs x= Injection (f x -> Sum f xs)

injections :: forall c f xs . SList c xs -> Product (Injection f xs) xs
injections (SCons rs) = Cons
  (Injection Zero)
  (mapProduct rs shiftInjection $ injections @c @f rs)
injections SNil = Nil

shiftInjection :: Injection f xs x -> Injection f (y : xs) x
shiftInjection (Injection inj) = Injection $ Suc . inj

applyInjections :: forall c f xs . SList c xs
                -> Product f xs
                -> Product (Const (Sum f xs)) xs
applyInjections rs args = zipProduct rs
  (\(Injection inj) arg -> Const (inj arg))
  (injections @c @f rs)
  args

genum :: SList ((~) '[]) xs -> [Sum (Product Identity) xs]
genum rs = collapseProduct rs (applyInjections rs (pureProduct rs Nil))

testEx17 :: IO ()
testEx17 = putStrLn $ "enum test: Colour: " <> show (enum @Colour)
