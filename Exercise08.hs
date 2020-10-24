{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Exercise08 where

import Module1
import Exercise07
import Data.Proxy   ( Proxy (..) )
import GHC.TypeLits ( Symbol, KnownSymbol, symbolVal )
import Prelude hiding ( show )
import qualified Prelude

class GShow a where
  gshow :: a -> String

instance (GShow a, GShow b) => GShow (Either a b) where
  gshow (Left  x) = gshow x
  gshow (Right x) = gshow x

instance (KnownSymbol n, GShow a) => GShow (Con n a) where
  gshow (Con x) = symbolVal (Proxy @n) <> " {" <> gshow x <> "}"

instance (GShow a, GShow b) => GShow (a, b) where
  gshow (x, y) = concat [gshow x, ", ", gshow y]

instance (KnownSymbol n, GShow a) => GShow (Sel n a) where
  gshow (Sel x) = symbolVal (Proxy @n) <> " = " <> gshow x

instance Show a => GShow (Wrap a) where
  gshow (Wrap x) = Prelude.show x

show :: (Generic a, GShow (Rep a)) => a -> String
show = gshow . from

testEx08 :: IO ()
testEx08 = putStrLn
  $ "show test: " <> Prelude.show test <> ": " <> show test
