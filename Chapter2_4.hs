{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Chapter2_4 where

import Data.Kind         ( Constraint, Type )
import Data.Proxy        ( Proxy (..) )
import GHC.TypeLits      ( KnownSymbol, Symbol )

data Representation
  = Sum         Representation Representation
  | Constructor Symbol         Representation
  | Product     Representation Representation
  | Unit
  | Atom        Type

type family Interpret (r :: Representation) :: Type where
  Interpret (Sum l r)         = Either (Interpret l) (Interpret r)
  Interpret (Constructor n x) = Interpret x
  Interpret (Product l r)     = (Interpret l, Interpret r)
  Interpret Unit              = ()
  Interpret (Atom x)          = x

type Rep a = Interpret (Code a)

class Generic (a :: Type) where
  type Code (a :: Type) :: Representation

  from :: a -> Rep a
  to   :: Rep a -> a

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

data SRepresentation (c :: Type -> Constraint) (r :: Representation) where
  SSum         :: SRepresentation c l
               -> SRepresentation c r
               -> SRepresentation c (Sum l r)

  SConstructor :: KnownSymbol n
               => Proxy n
               -> SRepresentation c a
               -> SRepresentation c (Constructor n a)

  SProduct     :: SRepresentation c l
               -> SRepresentation c r
               -> SRepresentation c (Product l r)

  SUnit        :: SRepresentation c Unit

  SAtom        :: c a
               => SRepresentation c (Atom a)

class IsRepresentation (c :: Type -> Constraint) (r :: Representation) where
  representation :: SRepresentation c r

instance (IsRepresentation c l, IsRepresentation c r)
  => IsRepresentation c (Sum l r) where
  representation = SSum representation representation

instance (IsRepresentation c a, KnownSymbol n)
  => IsRepresentation c (Constructor n a) where
  representation = SConstructor Proxy representation

instance (IsRepresentation c l, IsRepresentation c r)
  => IsRepresentation c (Product l r) where
  representation = SProduct representation representation

instance IsRepresentation c Unit where
  representation = SUnit

instance c a => IsRepresentation c (Atom a) where
  representation = SAtom

class Top a
instance Top a

type family IsEnumRepresentation (r :: Representation) :: Constraint where
  IsEnumRepresentation (Sum         l r) = ( IsEnumRepresentation l
                                           , IsEnumRepresentation r
                                           )
  IsEnumRepresentation (Constructor _ a) = a ~ Unit

type IsEnumType a = (Generic a, IsEnumRepresentation (Code a))

genum :: IsEnumRepresentation a => SRepresentation Top a -> [Interpret a]
genum (SSum l r) = (Left <$> genum l) ++ (Right <$> genum r)
genum (SConstructor _ a) = [()]

enum :: forall a . (IsEnumType a, IsRepresentation Top (Code a)) => [a]
enum = to <$> genum (representation @Top @(Code a))

testCh2_4 :: IO ()
testCh2_4 = putStrLn $ "enum test: Colour: " <> show (enum @Colour)
