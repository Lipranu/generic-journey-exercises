{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

module Exercise03 where

import Module1

data Test a
  = Test
  | Base a
  | OneMore
  deriving Show

instance Generic (Test a) where
  type Rep (Test a) = Either () (Either (Wrap a) ())

  from Test     = Left ()
  from (Base x) = Right $ Left $ Wrap x
  from OneMore  = Right $ Right ()

  to (Left _)                = Test
  to (Right (Left (Wrap x))) = Base x
  to (Right (Right _))       = OneMore

class GBaseCase a where
  gbaseCase :: [a]

instance GBaseCase () where
  gbaseCase = [()]

instance GBaseCase (Wrap a) where
  gbaseCase = []

instance (GBaseCase a, GBaseCase b) => GBaseCase (Either a b) where
  gbaseCase = (Left <$> gbaseCase) ++ (Right <$> gbaseCase)

baseCase :: (Generic a, GBaseCase (Rep a)) => [a]
baseCase = to <$> gbaseCase

testEx03 :: IO ()
testEx03 = putStrLn $ "baseCase test: " <> show (baseCase @(Test Int))
