{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exercise06 where

import Chapter1
import Data.Proxy   ( Proxy (..) )
import GHC.TypeLits ( KnownSymbol, symbolVal )

class GConParse a where
  gConParse :: String -> Maybe a

instance GConParse () where
  gConParse _ = Just ()

instance (GConParse a, KnownSymbol n) => GConParse (Con n a) where
  gConParse s | s == symbolVal (Proxy @n) = Con <$> gConParse s
              | otherwise = Nothing

instance (GConParse a, GConParse b) => GConParse (Either a b) where
  gConParse s = case Left <$> gConParse s of
    Nothing -> Right <$> gConParse s
    just    -> just

conParse :: (Generic a, GConParse (Rep a)) => String -> Maybe a
conParse s = to <$> gConParse s

testEx06 :: IO ()
testEx06 = do
  test "Red"
  test "Green"
  test "Blue"
  test "Purple"
  where test s = putStrLn
               $ "conParse test: " <> s <> ": " <> show (conParse @Colour s)
