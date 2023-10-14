module Dojang.Commands (printStderr) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import System.IO (stderr)

import Data.Text (Text)
import Data.Text.IO (hPutStrLn)


printStderr :: (MonadIO m) => Text -> m ()
printStderr = liftIO . hPutStrLn stderr
