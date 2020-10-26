{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Exercise13 where

import Chapter2_4   hiding ( IsEnumType, IsEnumRepresentation, enum, genum )
import Data.Kind           ( Constraint )
import GHC.TypeLits        ( TypeError, ErrorMessage (..))

type family IsEnumRepresentation (r :: Representation) :: Constraint where
  IsEnumRepresentation (Sum         l r) = ( IsEnumRepresentation l
                                           , IsEnumRepresentation r
                                           )
  IsEnumRepresentation (Constructor _ a) = IsEnumRepresentation a
  IsEnumRepresentation Unit              = ()
  IsEnumRepresentation a                 = TypeError (IsEnumError a)

type IsEnumError (a :: Representation)
     = Text "Only units and sums of units are allowed in Enum"
  :$$: Text " representation, but received "
  :<>: ShowType a

type IsEnumType a = (Generic a, IsEnumRepresentation (Code a))

genum :: IsEnumRepresentation a => SRepresentation Top a -> [Interpret a]
genum (SSum l r) = (Left <$> genum l) ++ (Right <$> genum r)
genum (SConstructor _ a) = genum a
genum SUnit = [()]

enum :: forall a . (IsEnumType a, IsRepresentation Top (Code a)) => [a]
enum = to <$> genum (representation @Top @(Code a))

testEx13 :: IO ()
testEx13 = putStrLn $ "enum test: Colour: " <> show (enum @Colour)
