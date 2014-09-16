module Conf.ParseSpec (spec) where

import           Data.Version (Version(..))
import           Distribution.Compiler (CompilerId(..), CompilerFlavor(..))
import           Distribution.PackageDescription (FlagName(..))
import           Distribution.System (OS(..), Arch(..))
import           Test.Hspec

import           Conf.Parse (Conf(..))
import qualified Conf.Parse as Conf


spec :: Spec
spec = do
  describe "parse" $ do
    it "accepts empty sections" $
      Conf.parse "" `shouldBe` Right (Conf.defaultConf)

    it "accepts OSes Cabal considers valid" $
      Conf.parse "windows" `shouldBe` Right (Conf.defaultConf { os = Windows })

    it "accepts arches Cabal considers valid" $
      Conf.parse ";i386" `shouldBe` Right (Conf.defaultConf { arch = I386 })

    it "accepts compilers Cabal considers valid" $
      Conf.parse ";;yhc-1.0"
     `shouldBe`
      Right (Conf.defaultConf { impl = CompilerId YHC Version { versionBranch = [1, 0], versionTags = [] } })

    it "accepts flags assignments" $
      Conf.parse ";;;+foo,-bar,-baz"
     `shouldBe`
      Right (Conf.defaultConf { cflags = [enable "foo", disable "bar", disable "baz" ] })

    it "acceots skipped sections" $
      Conf.parse ";i386;;+foo,-bar,-baz"
     `shouldBe`
      Right (Conf.defaultConf { arch = I386, cflags = [enable "foo", disable "bar", disable "baz" ] })

    it "accepts more skipped sections" $
      Conf.parse "windows;;yhc-1.0;"
     `shouldBe`
      Right (Conf.defaultConf {
          os   = Windows
        , impl = CompilerId YHC Version { versionBranch = [1, 0], versionTags = [] }
        })

    it "flag assignments start with + or -" $
      Conf.parse "windows;;yhc-1.0;*foo" `shouldSatisfy` isLeft

enable, disable :: String -> (FlagName, Bool)
enable n = (FlagName n, True)
disable n = (FlagName n, False)

isLeft :: Either e a -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False
