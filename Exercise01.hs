{-# LANGUAGE TypeFamilies #-}

module Exercise01 where

import Module1

data Term
  = App Term Term
  | Abs String Term
  | Var String
  deriving Show

instance Generic Term where
  type Rep Term = Either
    (Wrap Term, Wrap Term)
    (Either (Wrap String, Wrap Term) (Wrap String))

  from (App t1 t2) = Left (Wrap t1, Wrap t2)
  from (Abs s t)   = Right $ Left (Wrap s, Wrap t)
  from (Var s)     = Right $ Right $ Wrap s

  to (Left (Wrap t1, Wrap t2))       = App t1 t2
  to (Right (Left (Wrap s, Wrap t))) = Abs s t
  to (Right (Right (Wrap s)))        = Var s

instance Eq Term where
  (==) = eq

term1, term2, term3 :: Term
term1 = App term2 term3
term2 = Abs "abs" term3
term3 = Var "var"

testEx1 :: IO ()
testEx1 = do
  test term1 term2
  test term1 term3
  test term2 term3
  test term1 term1
  test term2 term2
  test term3 term3
  where test x y = putStrLn
                 $ "eq test: "
                <> show x
                <> " == "
                <> show y
                <> ": "
                <> show (x == y)
