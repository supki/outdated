{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Conf.Parse
  ( Conf(..)
  , parse
  , unparse
  , defaultConf
  ) where

import           Data.Bifunctor (first)
import           Data.Foldable (asum)
import           Data.Functor.Identity (Identity)
import           Data.List (intercalate)
import           Distribution.Compat.ReadP (readP_to_S)
import           Distribution.PackageDescription (FlagName(..), FlagAssignment)
import           Distribution.Compiler (CompilerId, buildCompilerId)
import           Distribution.System (OS, buildOS, Arch, buildArch)
import qualified Distribution.Text as Cabal
import           Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec


data Conf = Conf
  { os     :: OS
  , arch   :: Arch
  , impl   :: CompilerId
  , cflags :: FlagAssignment -- ^ Confguration flag assignment
  , pflags :: FlagAssignment -- ^ A default package flag assignment
  } deriving (Show, Eq)

defaultConf :: Conf
defaultConf = Conf { os = buildOS, arch = buildArch, impl = buildCompilerId, cflags = [], pflags = [] }

parse :: String -> Either String Conf
parse = first show . Parsec.parse parser "(outdated)"

parser :: Stream s Identity Char => Parsec s u Conf
parser = do
  os  <- _OS <|> return (os defaultConf)
  rec <- (char ';' >> parser') <|> return defaultConf
  return rec { os }

parser' :: Stream s Identity Char => Parsec s u Conf
parser' = do
  arch <- _Arch <|> return (arch defaultConf)
  rec  <- (char ';' >> parser'') <|> return defaultConf
  return rec { arch }

parser'' :: Stream s Identity Char => Parsec s u Conf
parser'' = do
  impl <- _Impl <|> return (impl defaultConf)
  rec  <- (char ';' >> parser''') <|> return defaultConf
  return rec { impl }

parser''' :: Stream s Identity Char => Parsec s u Conf
parser''' = _Flags >>= \cflags -> return defaultConf { cflags }


-- | Parse OS name
_OS :: Stream s Identity Char => Parsec s u OS
_OS = do
  raw <- many (satisfy (/= ';'))
  case fromText raw of
    Just x  -> return x
    Nothing -> fail ("Bad OS ‘" ++ raw ++ "’: please consult the Cabal manual for valid values")

-- | Parse architecture name
_Arch :: Stream s Identity Char => Parsec s u Arch
_Arch = do
  raw <- many (satisfy (/= ';'))
  case fromText raw of
    Just x  -> return x
    Nothing -> fail ("Bad arch ‘" ++ raw ++ "’: please consult the Cabal manual for valid values")

-- | Parse compiler implementation
_Impl :: Stream s Identity Char => Parsec s u CompilerId
_Impl = do
  raw <- many (satisfy (/= ';'))
  case fromText raw of
    Just x  -> return x
    Nothing -> fail ("Bad compiler ‘" ++ raw ++ "’: please consult the Cabal manual for valid values")

-- | Parse flags
_Flags :: Stream s Identity Char => Parsec s u FlagAssignment
_Flags = asum
  [ eof >> return []
  , _Flag `sepBy1` char ','
  ]

-- | Parse flag assignment
_Flag :: Stream s Identity Char => Parsec s u (FlagName, Bool)
_Flag = asum
  [ char '+' >> _FlagName >>= \n -> return (n, True)
  , char '-' >> _FlagName >>= \n -> return (n, False)
  ]

-- | Parse flag name
_FlagName :: Stream s Identity Char => Parsec s u FlagName
_FlagName = fmap FlagName (many1 (satisfy (`notElem` [',', ';'])))

fromText :: Cabal.Text a => String -> Maybe a
fromText s =
  case readP_to_S Cabal.parse s of
    [] -> Nothing
    xs -> case last xs of
      (x, "") -> Just x
      _       -> Nothing

unparse :: Conf -> String
unparse c = intercalate ";" $
  [ Cabal.display (os c)
  , Cabal.display (arch c)
  , Cabal.display (impl c)
  , intercalate "," (map prettyFlag (cflags c))
  ]
 where
  prettyFlag (FlagName n, b) = (if b then '+' else '-') : n
