{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exercise07 where

import Shared
import Data.Proxy   ( Proxy (..) )
import GHC.TypeLits ( Symbol, KnownSymbol, symbolVal )

newtype Sel (n :: Symbol) a = Sel a

data Test = Test
  { integer :: Integer
  , double  :: Double
  , string  :: String
  } deriving Show

instance Generic Test where
  type Rep Test = Con "Test" (Sel "integer" (Wrap Integer)
                             , (Sel "double" (Wrap Double)
                               , Sel "string" (Wrap String)
                               )
                             )

  from (Test x y z) = Con (Sel (Wrap x), (Sel (Wrap y), Sel (Wrap z)))

  to (Con (Sel (Wrap x), (Sel (Wrap y), Sel (Wrap z)))) = Test x y z

class GSelNames a where
  gSelNames :: a -> [String]

instance (GSelNames a, GSelNames b) => GSelNames (a, b) where
  gSelNames (x, y) = gSelNames x ++ gSelNames y

instance (GSelNames a, GSelNames b) => GSelNames (Either a b) where
  gSelNames (Left  x) = gSelNames x
  gSelNames (Right x) = gSelNames x

instance (GSelNames a) => GSelNames (Con n a) where
  gSelNames (Con x) = gSelNames x

instance KnownSymbol n => GSelNames (Sel n a) where
  gSelNames _ = [symbolVal $ Proxy @n]

selNames :: (Generic a, GSelNames (Rep a)) => a -> [String]
selNames = gSelNames . from

testEx07 :: IO ()
testEx07 = putStrLn $ "selNames test: " <> show x <> ": " <> show (selNames x)
  where x = Test 1 1.0 "1"
