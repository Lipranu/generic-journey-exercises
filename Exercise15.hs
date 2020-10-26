{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Exercise15 where

import Chapter2_8
import Data.Kind ( Constraint, Type )

type family IsEnumRepresentation (r :: SumRepresentation) :: Constraint where
  IsEnumRepresentation (x : xs) = (x ~ '[], IsEnumRepresentation xs)
  IsEnumRepresentation '[]      = ()

type IsEnumType a = (Generic a, IsEnumRepresentation (Code a))

genum :: IsEnumRepresentation r => SList Top r -> [InterpretSum r]
genum (SCons r) = Left () : (Right <$> genum r)
genum SNil = []

enum :: forall a . (IsEnumType a, IsList Top (Code a)) => [a]
enum = to <$> genum (list @_ @Top @(Code a))

testEx15 :: IO ()
testEx15 = putStrLn $ "enum test: Colour: " <> show (enum @Colour)
