{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Chapter2_6 where

import Data.Void    ( Void )
import Data.Kind    ( Constraint, Type )

type Rep a = Interpret (Code a)

class Generic (a :: Type) where
  type Code (a :: Type) :: Representation

  from :: a -> Rep a
  to   :: Rep a -> a

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving Show

instance Generic (Tree a) where
  type Code (Tree a) = ConsSum
    (ConsProduct a NilProduct)
    (ConsSum (ConsProduct (Tree a)
             (ConsProduct (Tree a) NilProduct))
    NilSum)

  from (Leaf x)   = Left (x, ())
  from (Node l r) = Right $ Left (l, (r, ()))

  to (Left (x, ()))              = Leaf x
  to (Right (Left (l, (r, ())))) = Node l r

instance Eq a => Eq (Tree a) where
  (==) = eq

data Colour
  = Red
  | Green
  | Blue
  deriving Show

instance Generic Colour where
  type Code Colour = ConsSum NilProduct
                   ( ConsSum NilProduct
                   ( ConsSum NilProduct NilSum))

  from Red   = Left ()
  from Green = Right $ Left  ()
  from Blue  = Right $ Right $ Left ()

  to (Left ())                 = Red
  to (Right (Left ()))         = Green
  to (Right (Right (Left ()))) = Blue

instance Eq Colour where
  (==) = eq

data SumRepresentation
  = ConsSum ProductRepresentation SumRepresentation
  | NilSum

data ProductRepresentation
  = ConsProduct Type ProductRepresentation
  | NilProduct

type family InterpretSum (r :: SumRepresentation) :: Type where
  InterpretSum (ConsSum x xs) = Either (InterpretProduct x) (InterpretSum xs)
  InterpretSum NilSum         = Void

type family InterpretProduct (r :: ProductRepresentation) :: Type where
  InterpretProduct (ConsProduct x xs) = (x, InterpretProduct xs)
  InterpretProduct NilProduct         = ()

type Representation = SumRepresentation
type Interpret a = InterpretSum a

data SSumRepresentation (c :: Type -> Constraint)
                        (r :: SumRepresentation) where
  SConsSum :: SProductRepresentation c x
           -> SSumRepresentation c xs
           -> SSumRepresentation c (ConsSum x xs)

  SNilSum  :: SSumRepresentation c NilSum

data SProductRepresentation (c :: Type -> Constraint)
                            (a :: ProductRepresentation) where
  SConsProduct :: c x
               => SProductRepresentation c xs
               -> SProductRepresentation c (ConsProduct x xs)

  SNilProduct  :: SProductRepresentation c NilProduct

class IsSumRepresentation (c :: Type -> Constraint)
                          (r :: SumRepresentation) where
  sumRepresentation :: SSumRepresentation c r

instance (IsProductRepresentation c x, IsSumRepresentation c xs)
  => IsSumRepresentation c (ConsSum x xs) where
  sumRepresentation = SConsSum productRepresentation sumRepresentation

instance IsSumRepresentation c NilSum where
  sumRepresentation = SNilSum

class IsProductRepresentation (c :: Type -> Constraint)
                              (r :: ProductRepresentation) where
  productRepresentation :: SProductRepresentation c r

instance (c x, IsProductRepresentation c xs)
  => IsProductRepresentation c (ConsProduct x xs) where
  productRepresentation = SConsProduct productRepresentation

instance IsProductRepresentation c NilProduct where
  productRepresentation = SNilProduct


type family IsEnumRepresentation (r :: Representation) :: Constraint where
  IsEnumRepresentation (ConsSum x xs) = ( x ~ NilProduct
                                        , IsEnumRepresentation xs
                                        )
  IsEnumRepresentation NilSum         = ()

type IsEnumType a = (Generic a, IsEnumRepresentation (Code a))

class Top a
instance Top a

eq :: forall a . (Generic a, IsSumRepresentation Eq (Code a))
   => a
   -> a
   -> Bool
eq x y = geqSum (sumRepresentation @Eq @(Code a)) (from x) $ from y

geqSum :: SSumRepresentation Eq a
       -> InterpretSum a
       -> InterpretSum a
       -> Bool
geqSum (SConsSum r _) (Left   x) (Left   y) = geqProduct r x y
geqSum (SConsSum _ r) (Right xs) (Right ys) = geqSum r xs ys
geqSum (SConsSum _ _) _          _          = False
geqSum SNilSum        _          _          = True

geqProduct :: SProductRepresentation Eq a
           -> InterpretProduct a
           -> InterpretProduct a
           -> Bool
geqProduct (SConsProduct r) (x, xs) (y, ys) = x == y && geqProduct r xs ys
geqProduct SNilProduct      ()      ()      = True

enum :: forall a . (IsEnumType a, IsSumRepresentation Top (Code a)) => [a]
enum = to <$> genum (sumRepresentation @Top @(Code a))

genum :: IsEnumRepresentation a
      => SSumRepresentation Top a
      -> [InterpretSum a]
genum (SConsSum _ xs) = Left () : (Right <$> genum xs)
genum SNilSum = []

testEq = do
  test tree1 tree2
  test tree1 tree1
  test tree2 tree2
  test Red   Blue
  test Green Blue
  test Green Red
  test Blue  Blue
  where tree1    = Node (Leaf 1) (Leaf 2)
        tree2    = Leaf 1
        test x y = putStrLn $ "eq test: "
                <> show x <> " == " <> show y <> ": " <> show (x == y)

testEnum :: IO ()
testEnum = putStrLn $ "enum test: Colour: " <> show (enum @Colour)

testCh2_6 :: IO ()
testCh2_6 = testEq >> testEnum
