{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  , Arg(..)
  , foldArg
  , GitHub(..)
  , displayGitHub
  ) where

import           Control.Applicative
import           Data.Foldable (asum)
import qualified Data.List as List
import           Data.Version (showVersion)
import           Distribution.PackageDescription (GenericPackageDescription(..), Flag(..), FlagAssignment)
import           Options.Applicative
import           Text.Printf (printf)

import           Conf.Parse (Conf(..), defaultConf, parse, unparse)
import           Paths_outdated (version)


cli :: IO ([Conf], [Arg])
cli = customExecParser (prefs (showHelpOnError <> columns 120)) cliParser

data Arg
  = ArgGitHub GitHub
  | ArgDirectory FilePath
  | ArgFile FilePath
    deriving (Show, Eq)

foldArg :: (GitHub -> a) -> (FilePath -> a) -> (FilePath -> a) -> Arg -> a
foldArg f g h = \case
  ArgGitHub x -> f x
  ArgDirectory x -> g x
  ArgFile x -> h x

data GitHub = GitHub
  { gitHubOwner   :: String
  , gitHubProject :: String
  } deriving (Show, Eq)

displayGitHub :: GitHub -> String
displayGitHub GitHub {gitHubOwner, gitHubProject} =
  printf "https://github.com/%s/%s" gitHubOwner gitHubProject

cliParser :: ParserInfo ([Conf], [Arg])
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
    <*> some
      (argument (fmap ArgGitHub url <|> fmap ArgFile file <|> fmap ArgDirectory dir)
                (metavar "URL/FILEPATH" <> help "GitHub project URL or a local .cabal file path"))

options :: ReadM a -> Mod OptionFields a -> Parser [a]
options r = some . option r

url :: ReadM GitHub
url = eitherReader $ \s -> do
  s' <- note ("Not a GitHub URL: ‘" ++ s ++ "’") (List.stripPrefix gitHub s)
  case breakOn (== '/') s' of
    [gitHubOwner, gitHubProject]
      -> Right GitHub {gitHubOwner, gitHubProject}
    _ -> Left ("Not a GitHub project URL: ‘" ++ s ++ "’")
 where
  gitHub = "https://github.com/"

file :: ReadM FilePath
file = eitherReader $ \s ->
  s <$ note ("Not a Cabal file: ‘" ++ s ++ "’") (stripSuffix ".cabal" s)
 where
  stripSuffix suffix path =
    fmap reverse (List.stripPrefix (reverse suffix) (reverse path))

dir :: ReadM FilePath
dir = str

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

breakOn :: (a -> Bool) -> [a] -> [[a]]
breakOn p = go
 where
  go xs = case break p xs of
    (x, []) -> [x]
    (x, y : ys)
      | p y       -> x : go ys
      | otherwise -> error "Conf.breakOn: impossible!"

formConf :: Conf -> GenericPackageDescription -> Conf
formConf c pd = c { pflags = packageFlagAssignment pd }

packageFlagAssignment :: GenericPackageDescription -> FlagAssignment
packageFlagAssignment = map (\f -> (flagName f, flagDefault f)) . genPackageFlags

flags :: Conf -> FlagAssignment
flags c = cflags c ++ pflags c
