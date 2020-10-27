{-# LANGUAGE GADTs      #-}
{-# LANGUAGE Rank2Types #-}

module Exercise18 where

import Chapter2_12
import Data.Coerce        ( coerce )
import Data.Functor.Const ( Const (..) )

collapseSum :: SList c xs -> Sum (Const a) xs -> a
collapseSum (SCons r) (Suc xs) = collapseSum r xs
collapseSum (SCons _) (Zero x) = coerce x
