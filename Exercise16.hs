{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Exercise16 where

import Chapter2_11
import Data.Functor.Identity ( Identity (..) )

data Term
  = App Term   Term
  | Abs String Term
  | Var String
  deriving Show

instance Generic Term where
  type Code Term = [ '[Term, Term], '[String, Term], '[String] ]

  from (App l r) = Zero (Identity l `Cons` (Identity r `Cons` Nil))
  from (Abs s t) = Suc (Zero (Identity s `Cons` (Identity t `Cons` Nil)))
  from (Var s)   = Suc (Suc (Zero (Identity s `Cons` Nil)))

  to (Zero (Identity l `Cons` (Identity r `Cons` Nil)))       = App l r
  to (Suc (Zero (Identity s `Cons` (Identity t `Cons` Nil)))) = Abs s t
  to (Suc (Suc (Zero (Identity s `Cons` Nil))))               = Var s
