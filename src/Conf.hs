{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
module Conf
  ( cli
  , unparse
  , formConf
#ifdef TEST
  , Conf(..), flags
  , cliParser
  , defaultConf
  , parse
#else
  , Conf, os, arch, impl, cflags, flags
#endif
  ) where

import           Control.Applicative
import           Data.Foldable (asum)
import           Data.Monoid (mconcat)
import           Data.Version (showVersion)
import           Distribution.PackageDescription (GenericPackageDescription(..), Flag(..), FlagAssignment)
import           Options.Applicative

import           Conf.Parse (Conf(..), defaultConf, parse, unparse)
import           Paths_outdated (version)


cli :: IO ([Conf], [FilePath])
cli = customExecParser (prefs (showHelpOnError <> columns 120)) cliParser

cliParser :: ParserInfo ([Conf], [FilePath])
cliParser = info (helper <*> parser) (mconcat
  [ fullDesc
  , progDesc "Does your package accept the latest versions of its dependencies?"
  , header ("outdated " ++ showVersion version)
  ])
 where
  parser = (,)
    <$> asum
      [ options (eitherReader parse) (mconcat
        [ short 'c'
        , long "conf"
        , metavar "CONF"
        , help ("Configuration to check (default: ‘" ++ unparse defaultConf ++ "’)")
        ])
      , pure [defaultConf]
      ]
    <*> arguments (metavar "CABAL" <> help "Path to .cabal file")

options :: (String -> ReadM a) -> Mod OptionFields a -> Parser [a]
options r = some . option r

arguments :: Mod ArgumentFields String -> Parser [String]
arguments = some . strArgument

formConf :: Conf -> GenericPackageDescription -> Conf
formConf c pd = c { pflags = packageFlagAssignment pd }

packageFlagAssignment :: GenericPackageDescription -> FlagAssignment
packageFlagAssignment = map (\f -> (flagName f, flagDefault f)) . genPackageFlags

flags :: Conf -> FlagAssignment
flags c = cflags c ++ pflags c
