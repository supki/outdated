{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
module Wrong
  ( Wrong(..)
  , CabalFileError(..)
  , Problem(..)
  , Target(..)
  , produce
  , prettify
  ) where

import           Control.Exception (IOException, ErrorCall, catches, Handler(..), evaluate)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy as ByteString
import           Data.Conduit (Source)
import qualified Data.Conduit as C
import           Data.Foldable (for_, toList)
import           Data.Function (on)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Version (Version(..), showVersion)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Distribution.Compiler (CompilerId(..))
import           Distribution.InstalledPackageInfo (PError)
import           Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName(..))
import           Distribution.PackageDescription
  ( GenericPackageDescription(..), PackageDescription(..)
  , CondTree(..), Condition(..), ConfVar(..)
  )
import           Distribution.PackageDescription.Parse (ParseResult(..), parsePackageDescription)
import           Distribution.Text (display)
import           Distribution.Version (VersionRange, intersectVersionRanges, withinRange)
import qualified Network.HTTP.Conduit as Http
import           System.Directory (getDirectoryContents)
import           System.FilePath ((</>), (<.>), combine)
import           Text.Printf (printf)

import           Conf (Conf, GitHub(..))
import qualified Conf
import           Latest (Latest)
import qualified Latest


data Wrong
  = WrongCabal FilePath CabalFileError
  | WrongConf FilePath Conf (NonEmpty (NonEmpty Problem))
    deriving
    ( Show
#if __GLASGOW_HASKELL__ >= 708
    , Eq
#endif
    )

data CabalFileError
  = IO IOException
  | Error ErrorCall
  | Parse PError
    deriving
    ( Show
#if __GLASGOW_HASKELL__ >= 708
    , Eq
#endif
    )

data Problem = Problem Target PackageName VersionRange (Maybe Version)
    deriving (Show, Eq)

data Target = Library | Executable String | TestSuite String | Benchmark String
    deriving (Show, Eq)

produce :: MonadIO m => FilePath -> [Conf] -> [Conf.Arg] -> Latest PackageName Version -> Source m Wrong
produce tmpDir confs args index =
  for_ args $ \arg -> do
    files <- Conf.foldArg (fmap pure . downloadCabalFile tmpDir) listCabalFiles (pure . pure) arg
    for_ files $ \file -> do
      ed <- readCabalFile file
      case ed of
        Left  e -> C.yield (WrongCabal (Conf.foldArg Conf.displayGitHub (const file) (const file) arg) e)
        Right d ->
          for_ confs $ \conf -> do
            case askCabal index (Conf.formConf conf d) d of
              []       -> return ()
              (c : cs) -> C.yield (WrongConf (Conf.foldArg Conf.displayGitHub (const file) (const file) arg) conf (c :| cs))

prettify :: Wrong -> String
prettify = pp
 where
  pp (WrongCabal path (IO e))    = "Couldn't read ‘"  ++ path ++ "’: " ++ show e
  pp (WrongCabal path (Error e)) = "Couldn't parse ‘"  ++ path ++ "’: " ++ show e
  pp (WrongCabal path (Parse e)) = "Couldn't parse ‘" ++ path ++ "’: " ++ show e
  pp (WrongConf path conf xs)    = List.intercalate "\n"
    . (pconf path conf :) . flip concatMap (toList xs) $ \ys -> pcomponent (NonEmpty.head ys) : map pproblem (toList ys)

  pconf path conf = "‘" ++ path ++ "’ has outdated dependencies against ‘" ++ Conf.unparse conf ++ "’:"

  pcomponent (Problem Library _ _ _)        = "  - library"
  pcomponent (Problem (Executable n) _ _ _) = "  - executable ‘" ++ n ++ "’"
  pcomponent (Problem (TestSuite n) _ _ _)  = "  - test-suite ‘" ++ n ++ "’"
  pcomponent (Problem (Benchmark n) _ _ _)  = "  - benchmark ‘"  ++ n ++ "’"

  pproblem (Problem _ (PackageName n) _ Nothing) = "    ‘"  ++ n ++ "’ isn't in the index"
  pproblem (Problem _ (PackageName n) r (Just v)) =
    "    the version range of ‘"  ++ n ++ "’ (" ++ display r ++ ") does not include the latest version " ++ showVersion v

downloadCabalFile :: MonadIO m => FilePath -> GitHub -> m FilePath
downloadCabalFile tmpDir GitHub {gitHubOwner, gitHubProject} = liftIO $ do
  let tmpFile = tmpDir </> gitHubProject <.> "cabal"
  req <- Http.parseUrl cabalUrl
  man <- Http.newManager Http.tlsManagerSettings
  res <- Http.httpLbs req man
  liftIO (ByteString.writeFile tmpFile (Http.responseBody res))
  return tmpFile
 where
  cabalUrl = printf "https://raw.githubusercontent.com/%s/%s/master/%s.cabal" gitHubOwner gitHubProject gitHubProject

listCabalFiles :: MonadIO m => FilePath -> m [FilePath]
listCabalFiles dir =
  liftIO (fmap (map (combine dir) . filter (List.isSuffixOf ".cabal")) (getDirectoryContents dir))

readCabalFile :: MonadIO m => FilePath -> m (Either CabalFileError GenericPackageDescription)
readCabalFile f = liftIO $ do
  x <- Text.readFile f
  p <- evaluate (parsePackageDescription (Text.unpack x))
  case p of
    ParseFailed e -> return (Left (Parse e))
    ParseOk _ gpd -> return (Right gpd)
 `catches`
  [ Handler ioException
  , Handler errorCall
  ]

ioException :: IOException -> IO (Either CabalFileError a)
ioException = return . Left . IO

errorCall :: ErrorCall -> IO (Either CabalFileError a)
errorCall = return . Left . Error

askCabal :: Latest PackageName Version -> Conf -> GenericPackageDescription -> [NonEmpty Problem]
askCabal index conf gpd =
  let
    PackageIdentifier { pkgName = k, pkgVersion = v } = package (packageDescription gpd)
    index' = Latest.insert k v index
  in
    case concatMap (\f -> f index' conf gpd) [lib, exe, test, bench] of
      []       -> []
      (x : xs) -> NonEmpty.groupBy (equating component) (x :| xs)

equating :: Eq b => (a -> b) -> a -> a -> Bool
equating = on (==)

component :: Problem -> Target
component (Problem c _ _ _) = c

lib :: Latest PackageName Version -> Conf -> GenericPackageDescription -> [Problem]
lib = componentProblems (maybe [] (\x -> [((), x)]) . condLibrary) (\_ -> Library)

exe :: Latest PackageName Version -> Conf -> GenericPackageDescription -> [Problem]
exe = componentProblems condExecutables Executable

test :: Latest PackageName Version -> Conf -> GenericPackageDescription -> [Problem]
test = componentProblems condTestSuites TestSuite

bench :: Latest PackageName Version -> Conf -> GenericPackageDescription -> [Problem]
bench = componentProblems condBenchmarks Benchmark

componentProblems
  :: (a -> [(t, CondTree ConfVar [Dependency] a1)])
  -> (t -> Target)
  -> Latest PackageName Version
  -> Conf
  -> a
  -> [Problem]
componentProblems proj inj index conf =
  concatMap (\(n, xs) -> mapMaybe (badDependency (inj n) index) (ranges conf xs)) . proj

badDependency :: Target -> Latest PackageName Version -> Dependency -> Maybe Problem
badDependency c i (Dependency n r) =
  case Latest.lookup n i of
    Nothing -> Just (Problem c n r Nothing)
    Just v  -> if withinRange v r then Nothing else Just (Problem c n r (Just v))

ranges :: Conf -> CondTree ConfVar [Dependency] a -> [Dependency]
ranges conf = regroupRanges . complyingRanges conf

-- | Take intersection of the contraints on the same package
regroupRanges :: [Dependency] -> [Dependency]
regroupRanges = map t2d . Map.toList . Map.fromListWith intersectVersionRanges . map d2t
 where
  d2t (Dependency n r) = (n, r)
  t2d (n, r) = (Dependency n r)

-- | Retrieve the version constraints falling under the proposed configuration
complyingRanges :: Conf -> CondTree ConfVar [c] a -> [c]
complyingRanges c = go
 where
  go (CondNode _ xs ys) =
    xs ++ concatMap (\(p, us, mvs) -> if decideCondition c p then go us else maybe [] go mvs) ys

decideCondition :: Conf -> Condition ConfVar -> Bool
decideCondition c = go
 where
  go (Var v)    = decideConfVar c v
  go (Lit b)    = b
  go (CNot x)   = not (go x)
  go (COr x y)  = go x || go y
  go (CAnd x y) = go x && go y

decideConfVar :: Conf -> ConfVar -> Bool
decideConfVar c (OS x)     = x == Conf.os c
decideConfVar c (Arch x)   = x == Conf.arch c
decideConfVar c (Impl x r) = let CompilerId y v = Conf.impl c in x == y && v `withinRange` r
decideConfVar c (Flag x)   = maybe False id (lookup x (Conf.flags c))
