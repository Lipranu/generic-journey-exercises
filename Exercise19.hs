{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Exercise19 where

import Chapter2_13
import Data.Coerce           ( coerce )
import Data.Functor.Identity ( Identity (..) )
import qualified Data.Monoid as M

data MonoidTest a b c = MonoidTest a b c deriving Show

instance Generic (MonoidTest a b c) where
  type Code (MonoidTest a b c) = '[ '[a, b, c] ]

  from (MonoidTest x y z) = Zero (Identity x `Cons`
                                 (Identity y `Cons`
                                 (Identity z `Cons` Nil)))

  to (Zero (Identity x `Cons` (Identity y `Cons` (Identity z `Cons` Nil))))
    = MonoidTest x y z

instance (Semigroup a, Semigroup b, Semigroup c)
  => Semigroup (MonoidTest a b c) where (<>) = gmappend

instance (Monoid a, Monoid b, Monoid c)
  => Monoid (MonoidTest a b c) where mempty = gmempty

gmappend :: forall a xs . (IsProductType a xs, IsList Semigroup xs)
         => a -> a -> a
gmappend x y = to $ Zero $ go (from x) (from y)
  where go (Zero xs) (Zero ys) = zipProduct rs (\x y -> coerce (x <> y)) xs ys
        rs = list @_ @Semigroup

testGmempty :: IO ()
testGmempty = putStrLn $ "gmempty: " <> show m
  where m = mempty @(MonoidTest (M.Sum Integer) (M.Product Integer) M.Any)

testGmappend :: IO ()
testGmappend = do
  test m1 mempty
  test mempty m2
  test m2 m3
  test m3 m2
  where m1 = MonoidTest (M.Sum 1) (M.Product 2)  (M.Any True)
        m2 = MonoidTest (M.Sum 7) (M.Product 10) (M.Any False)
        m3 = MonoidTest (M.Sum 2) (M.Product 1)  (M.Any False)
        test x y = putStrLn $ "gmappend: "
                <> show x <> " <> " <> show y <> " = " <> show (x <> y)

testEx19 :: IO ()
testEx19 = testGmempty >> testGmappend
