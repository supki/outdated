{-# LANGUAGE TypeFamilies #-}
module Progress
  ( progress
  ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Conduit (Conduit)
import qualified Data.Conduit as C
import           Data.Function (fix)
import qualified System.IO as IO


progress :: (i ~ o, MonadIO m) => String -> Conduit i m o
progress m = do
  write m
  flip fix 0 $ \loop n -> do
    mx <- C.await
    case mx of
      Nothing -> write "\n"
      Just x  -> if n `divides` 100 then write "." >> C.yield x >> loop 1 else C.yield x >> loop (n + 1)

divides :: Int -> Int -> Bool
divides n m = n `rem` m == 0

write :: MonadIO m => String -> m ()
write m = liftIO (putStr m) >> flush

flush :: MonadIO m => m ()
flush = liftIO (IO.hFlush IO.stdout)
