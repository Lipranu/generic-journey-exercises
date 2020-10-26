{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Chapter2_5 where

import Data.Kind    ( Constraint, Type )
import Data.Proxy   ( Proxy (..) )
import GHC.TypeLits ( KnownSymbol, Symbol )

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
  type Code (Tree a) = Sum (Constructor "Leaf" (Atom a))
                           (Constructor "Node" (Product (Atom (Tree a))
                                                        (Atom (Tree a))))
  from (Leaf x)   = Left x
  from (Node l r) = Right (l, r)

  to (Left x)       = Leaf x
  to (Right (l, r)) = Node l r

instance Eq a => Eq (Tree a) where
  (==) = eq

data Colour
  = Red
  | Green
  | Blue
  deriving Show

instance Generic Colour where
  type Code Colour = Sum (Constructor "Red" Unit)
                         (Sum (Constructor "Green" Unit)
                              (Constructor "Blue" Unit))

  from Red   = Left ()
  from Green = Right $ Left  ()
  from Blue  = Right $ Right ()

  to (Left ())          = Red
  to (Right (Left  ())) = Green
  to (Right (Right ())) = Blue

instance Eq Colour where
  (==) = eq

data SumRepresentation
  = Sum SumRepresentation SumRepresentation
  | Constructor Symbol ProductRepresentation

data ProductRepresentation
  = Product ProductRepresentation ProductRepresentation
  | Unit
  | Atom Type

type family InterpretSum (r :: SumRepresentation) :: Type where
  InterpretSum (Sum         l r) = Either (InterpretSum l) (InterpretSum r)
  InterpretSum (Constructor n a) = InterpretProduct a

type family InterpretProduct (r :: ProductRepresentation) :: Type where
  InterpretProduct (Product l r) = (InterpretProduct l, InterpretProduct r)
  InterpretProduct Unit          = ()
  InterpretProduct (Atom a)      = a

type Representation = SumRepresentation
type Interpret a = InterpretSum a

data SSumRepresentation (c :: Type -> Constraint)
                        (r :: SumRepresentation) where
  SSum         :: SSumRepresentation c l
               -> SSumRepresentation c r
               -> SSumRepresentation c (Sum l r)

  SConstructor :: KnownSymbol n
               => Proxy n
               -> SProductRepresentation c r
               -> SSumRepresentation c (Constructor n r)

data SProductRepresentation (c :: Type -> Constraint)
                            (a :: ProductRepresentation) where
  SProduct :: SProductRepresentation c l
           -> SProductRepresentation c r
           -> SProductRepresentation c (Product l r)

  SUnit    :: SProductRepresentation c Unit

  SAtom    :: c a => SProductRepresentation c (Atom a)

class IsSumRepresentation (c :: Type -> Constraint)
                          (r :: SumRepresentation) where
  sumRepresentation :: SSumRepresentation c r

instance (IsSumRepresentation c l, IsSumRepresentation c r)
  => IsSumRepresentation c (Sum l r) where
  sumRepresentation = SSum sumRepresentation sumRepresentation

instance (IsProductRepresentation c r, KnownSymbol n)
  => IsSumRepresentation c (Constructor n r) where
  sumRepresentation = SConstructor Proxy productRepresentation

class IsProductRepresentation (c :: Type -> Constraint)
                              (r :: ProductRepresentation) where
  productRepresentation :: SProductRepresentation c r

instance (IsProductRepresentation c l, IsProductRepresentation c r)
  => IsProductRepresentation c (Product l r) where
  productRepresentation = SProduct productRepresentation
                                   productRepresentation

instance c r => IsProductRepresentation c (Atom r) where
  productRepresentation = SAtom

instance IsProductRepresentation c Unit where
  productRepresentation = SUnit

type family IsEnumRepresentation (r :: Representation) :: Constraint where
  IsEnumRepresentation (Sum         l r) = ( IsEnumRepresentation l
                                           , IsEnumRepresentation r
                                           )
  IsEnumRepresentation (Constructor _ a) = a ~ Unit

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
geqSum (SSum         l _) (Left  x) (Left  y) = geqSum l x y
geqSum (SSum         _ r) (Right x) (Right y) = geqSum r x y
geqSum (SSum         _ _) _         _         = False
geqSum (SConstructor _ a) x         y         = geqProduct a x y

geqProduct :: SProductRepresentation Eq a
           -> InterpretProduct a
           -> InterpretProduct a
           -> Bool
geqProduct (SProduct l r) (x1, y1) (x2, y2) = geqProduct l x1 x2
                                           && geqProduct r y1 y2
geqProduct SAtom          x        y        = x == y
geqProduct SUnit          ()       ()       = True

enum :: forall a . (IsEnumType a, IsSumRepresentation Top (Code a)) => [a]
enum = to <$> genum (sumRepresentation @Top @(Code a))

genum :: IsEnumRepresentation a
      => SSumRepresentation Top a
      -> [InterpretSum a]
genum (SSum         l r) = (Left <$> genum l) ++ (Right <$> genum r)
genum (SConstructor _ _) = [()]

testEq = do
  test tree1 tree2
  test tree1 tree1
  test tree2 tree2
  test Red   Blue
  test Green Blue
  test Green Red
  test Blue  Blue
  where
    tree1    = Node (Leaf 1) (Leaf 2)
    tree2    = Leaf 1
    test x y = putStrLn $ "eq test: "
            <> show x <> " == " <> show y <> ": " <> show (x == y)

testEnum :: IO ()
testEnum = putStrLn $ "enum test: Colour: " <> show (enum @Colour)

testCh2_5 :: IO ()
testCh2_5 = testEq >> testEnum
