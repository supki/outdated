module Latest
  ( Latest
  , gather
  , lookup
  , insert
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Conduit (ResumableSource, Sink, ($$+-), ($$), (=$=))
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Map.Strict as Map
import           Data.String (fromString)
import           Data.Version (Version(..))
import           Distribution.Package (PackageName(..))
import qualified Network.HTTP.Conduit as Http
import           Prelude hiding (lookup)
import qualified Prelude
import           System.IO.Error (catchIOError)

import           Latest.Parse (parseIndex)
import           Latest.Type (Latest(..))
import qualified Path
import           Progress (progress)


gather :: IO (Latest PackageName Version)
gather = do
  url <- Path.indexUrl
  Http.withManager $ \m -> do
    req <- Http.parseUrl url
    res <- Http.http req m
    case parseIndexSignature res of
      Nothing -> downloadIndex res
      Just sig -> do
        x <- loadIndexSignature
        case x of
          Nothing -> downloadIndex res <* saveIndexSignature sig
          Just sig'
            | sig == sig' -> loadIndex
            | otherwise   -> downloadIndex res <* saveIndexSignature sig

parseIndexSignature :: Http.Response b -> Maybe ByteString
parseIndexSignature = Prelude.lookup (fromString "Content-MD5") . Http.responseHeaders

saveIndex :: MonadResource m => Sink ByteString m ()
saveIndex = CB.sinkFile =<< Path.index

downloadIndex
  :: MonadResource m => Http.Response (ResumableSource m ByteString) -> m (Latest PackageName Version)
downloadIndex res =
  Http.responseBody res $$+-
    progress "Downloading the index.." =$= C.getZipSink (C.ZipSink saveIndex *> C.ZipSink parseIndex)

loadIndex :: MonadResource m => m (Latest PackageName Version)
loadIndex = do
  p <- Path.index
  CB.sourceFile p $$
    progress "Loading the index.." =$= parseIndex

saveIndexSignature :: MonadIO m => ByteString -> m ()
saveIndexSignature bs = Path.indexSignature >>= \p -> liftIO (ByteString.writeFile p bs)

loadIndexSignature :: MonadIO m => m (Maybe ByteString)
loadIndexSignature =
  liftIO ((Path.indexSignature >>= fmap Just . ByteString.readFile) `catchIOError` \_ -> return Nothing)

lookup :: Ord k => k -> Latest k v -> Maybe v
lookup k (Latest m) = Map.lookup k m

insert :: Ord k => k -> v -> Latest k v -> Latest k v
insert k v (Latest m) = Latest (Map.insert k v m)
