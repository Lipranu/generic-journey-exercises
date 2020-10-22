{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeApplications #-}

module Exercise04 where

import Shared
import Data.Monoid ( Sum (..), Product (..), All (..), Any (..) )

data Test = Test (Sum Integer) All Any (Product Integer) deriving Show

instance Generic Test where
  type Rep Test = (Wrap (Sum Integer)
                  , (Wrap All
                    , (Wrap Any
                      , Wrap (Product Integer))))

  from (Test x y z v) = (Wrap x, (Wrap y, (Wrap z, Wrap v)))
  to (Wrap x, (Wrap y, (Wrap z, Wrap v))) = Test x y z v

class GMemp a where
  gmemp :: a

instance Monoid a => GMemp (Wrap a) where
  gmemp = Wrap $ mempty

instance (GMemp a, GMemp b) => GMemp (a, b) where
  gmemp = (gmemp, gmemp)

memp :: (Generic a, GMemp (Rep a)) => a
memp = to $ gmemp
