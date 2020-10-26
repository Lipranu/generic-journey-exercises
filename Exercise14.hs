{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}

module Exercise14 where

import Chapter2_4
import Data.Kind  ( Constraint )

data Test a = Test a a a deriving Show

instance Generic (Test a) where
  type Code (Test a) = Constructor "Test"
    (Product (Atom a)
    (Product (Atom a)
             (Atom a)))

  from (Test x y z) = (x, (y, z))
  to   (x, (y, z))  = Test x y z

type family IsProductRepresentation (r :: Representation) :: Constraint where
  IsProductRepresentation (Constructor _ a) = IsProductRepresentation a
  IsProductRepresentation (Product     l r) = ( IsProductRepresentation l
                                              , IsProductRepresentation r
                                              )
  IsProductRepresentation (Atom          _) = ()
  IsProductRepresentation Unit              = ()

type IsProductType a = (Generic a, IsProductRepresentation (Code a))

gShowProd :: IsProductRepresentation a
          => SRepresentation Show a
          -> Interpret a
          -> [String]
gShowProd (SConstructor _ a) x      = gShowProd a x
gShowProd (SProduct     l r) (x, y) = gShowProd l x ++ gShowProd r y
gShowProd SAtom              x      = [show x]

showProd :: forall a . (IsProductType a, IsRepresentation Show (Code a))
         => a
         -> [String]
showProd = gShowProd (representation @Show @(Code a)) . from

testEx14 :: IO ()
testEx14 = putStrLn
         $ "showProd test: " <> show test <> ": " <> show (showProd test)
  where test = Test @Int 1 2 3
