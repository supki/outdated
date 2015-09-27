module Path
  ( index
  , indexSignature
  , indexUrl
  ) where

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           System.Directory (createDirectoryIfMissing, getAppUserDataDirectory)
import           System.Environment (lookupEnv)
import           System.FilePath ((</>))


-- | THe location of the cached Hackage tarball
index :: MonadIO m => m FilePath
index = outdated "index.tar.gz"

-- | The location of the cached Hackage tarball MD5 signature
--
-- Sorry, the MD5 fingerprint is the only thing Hackage provides
indexSignature :: MonadIO m => m FilePath
indexSignature = outdated "index.sig"

-- | The package index URL
indexUrl :: IO String
indexUrl = maybe "https://hackage.haskell.org/packages/index.tar.gz" id <$> lookupEnv "OUTDATED_INDEX_URL"


outdated :: MonadIO m => FilePath -> m FilePath
outdated p = liftM (</> p) outdatedData

outdatedData :: MonadIO m => m FilePath
outdatedData = liftIO $ do
  d <- maybe (getAppUserDataDirectory "outdated") return =<< lookupEnv "OUTDATED_HOME"
  createDirectoryIfMissing True d
  return d
