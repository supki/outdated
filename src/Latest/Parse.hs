module Latest.Parse
  ( parseIndex
  ) where

import           Control.Monad
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import           Data.Char (ord)
import           Data.Conduit (Conduit, Sink, (=$=))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Zlib as CZ
import           Data.List (unfoldr)
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))
import           Data.Version (Version, makeVersion)
import           Distribution.Package (PackageName(..))
import           Prelude hiding (lookup)

import           Latest.Type (Latest(..))


parseIndex :: MonadResource m => Sink ByteString m (Latest PackageName Version)
parseIndex = CZ.ungzip =$= peekIndex =$= fmap (Latest . Map.fromListWith max) CL.consume

peekIndex :: Monad m => Conduit ByteString m (PackageName, Version)
peekIndex = go 0 ByteString.empty
 where
  go 0 bs
    | ByteString.null bs = maybe pass (go 0) =<< C.await
    | ByteString.length bs < 135 = maybe pass (\bs' -> go 0 (bs <> bs')) =<< C.await
    | otherwise =
      let h = ByteString.take 135 bs
          p = fst (ByteString.break (== 0) h)
          s = parseSize h
      in unless (ByteString.null p) $ do
           C.yield (parsePath p)
           go (512 + (((s - 1) `quot` 512) + 1) * 512) bs
  go skip bs
    | ByteString.null bs = maybe pass (go skip) =<< C.await
    | otherwise = go (max 0 (skip - ByteString.length bs)) (ByteString.drop skip bs)

parseSize :: Integral a => ByteString -> a
parseSize =
    ByteString.foldl' (\a x -> a * 8 + fromIntegral x - fromIntegral (ord '0')) 0
  . ByteString.take 11
  . ByteString.drop 124

parsePath :: ByteString -> (PackageName, Version)
parsePath xs =
  case ByteString.break (== fromIntegral (ord '/')) xs of
    (ys, xs') -> case ByteString.break (== fromIntegral (ord '/')) (ByteString.drop 1 xs') of
      (zs, _) -> (PackageName (Char8.unpack ys), parseVersion zs)

parseVersion :: ByteString -> Version
parseVersion =
  makeVersion . unfoldr (fmap (fmap (ByteString.drop 1)) . Char8.readInt)

pass :: Monad m => m ()
pass = return ()
