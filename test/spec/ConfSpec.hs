module ConfSpec (spec) where

import           Data.Monoid (mempty)
import           Data.Version (Version(..))
import           Distribution.Compiler (CompilerId(..), CompilerFlavor(..))
import           Distribution.System (OS(..), Arch(..))
import qualified Options.Applicative as O
import           Test.Hspec

import           Conf (Conf(..))
import qualified Conf


spec :: Spec
spec = do
  describe "cliParser" $ do
    it "the list of arguments cannot be empty" $
      parse [] `shouldBe` Nothing

    it "the list of arguments should include at least one filepath" $
      parse ["-c", "linux;x86_64;ghc-7.8;"] `shouldBe` Nothing

    it "the list of arguments can include only filepaths" $
      paths (parse ["foo.cabal", "bar.cabal"]) `shouldBe` Just ["foo.cabal", "bar.cabal"]

    it "if the list of arguments includes only filepaths the default configuration is chosen" $
      confs (parse ["foo.cabal", "bar.cabal"]) `shouldBe` Just [Conf.defaultConf]

    it "the list of arguments can include a configuration description" $
      parse ["-c", "linux;x86_64;ghc-7.8;", "foo.cabal"] `shouldBe` Just ([linux_x8664_ghc78], ["foo.cabal"])

    it "the list of arguments can include multiple configuration descriptions" $
      parse ["--conf", "linux;x86_64;ghc-7.8;", "--conf", "windows;i386;ghc-7.0;", "foo.cabal"]
     `shouldBe`
      Just ([linux_x8664_ghc78, windows_i386_ghc70], ["foo.cabal"])

linux_x8664_ghc78 :: Conf
linux_x8664_ghc78 =
  Conf {
      os     = Linux
    , arch   = X86_64
    , impl   = CompilerId GHC Version { versionBranch = [7, 8], versionTags = [] }
    , cflags = []
    , pflags = []
  }

windows_i386_ghc70 :: Conf
windows_i386_ghc70 =
  Conf {
      os     = Windows
    , arch   = I386
    , impl   = CompilerId GHC Version { versionBranch = [7, 0], versionTags = [] }
    , cflags = []
    , pflags = []
  }

parse :: [String] -> Maybe ([Conf], [FilePath])
parse = O.getParseResult . O.execParserPure (O.prefs mempty) Conf.cliParser

paths :: Functor f => f (a, b) -> f b
paths = fmap snd

confs :: Functor f => f (a, b) -> f a
confs = fmap fst
