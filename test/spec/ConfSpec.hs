module ConfSpec (spec) where

import           Data.Version (makeVersion)
import           Distribution.Compiler (CompilerId(..), CompilerFlavor(..))
import           Distribution.System (OS(..), Arch(..))
import           Options.Applicative
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
      paths (parse ["foo.cabal", "bar.cabal"]) `shouldBe` Just [Right "foo.cabal", Right "bar.cabal"]

    it "if the list of arguments includes only filepaths the default configuration is chosen" $
      confs (parse ["foo.cabal", "bar.cabal"]) `shouldBe` Just [Conf.defaultConf]

    it "the list of arguments can include a configuration description" $
      parse ["-c", "linux;x86_64;ghc-7.8;", "foo.cabal"] `shouldBe` Just ([linux_x8664_ghc78], [Right "foo.cabal"])

    it "the list of arguments can include multiple configuration descriptions" $
      parse ["--conf", "linux;x86_64;ghc-7.8;", "--conf", "windows;i386;ghc-7.0;", "foo.cabal"]
     `shouldBe`
      Just ([linux_x8664_ghc78, windows_i386_ghc70], [Right "foo.cabal"])

linux_x8664_ghc78 :: Conf
linux_x8664_ghc78 =
  Conf {
      os     = Linux
    , arch   = X86_64
    , impl   = CompilerId GHC (makeVersion [7, 8])
    , cflags = []
    , pflags = []
  }

windows_i386_ghc70 :: Conf
windows_i386_ghc70 =
  Conf {
      os     = Windows
    , arch   = I386
    , impl   = CompilerId GHC (makeVersion [7, 0])
    , cflags = []
    , pflags = []
  }

parse :: [String] -> Maybe ([Conf], [Either Conf.GitHub FilePath])
parse = getParseResult . execParserPure (prefs mempty) Conf.cliParser

paths :: Functor f => f (a, b) -> f b
paths = fmap snd

confs :: Functor f => f (a, b) -> f a
confs = fmap fst
