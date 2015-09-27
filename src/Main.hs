module Main (main) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Conduit (Sink, (=$=), runConduit)
import qualified Data.Conduit as C
import           Data.Function (fix)
import           System.Exit (exitFailure, exitSuccess)

import qualified Conf
import qualified Latest
import qualified Wrong


main :: IO a
main = do
  (confs, paths) <- Conf.cli
  index <- Latest.gather
  runConduit $
    Wrong.produce confs paths index =$= C.mapInput Wrong.prettify (\_ -> Nothing) printAndDie

printAndDie :: MonadIO m => Sink String m a
printAndDie = flip fix True $ \loop r -> do
  mx <- C.await
  case mx of
    Nothing -> if r then liftIO (println "OK" >> exitSuccess) else liftIO exitFailure
    Just x  -> do println x; loop False

println :: MonadIO m => String -> m ()
println = liftIO . putStrLn
