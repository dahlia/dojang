module Options.Applicative.Path
  ( hyphen
  , path
  , pathOption
  , pathArgument
  , period
  ) where

import System.IO.Unsafe (unsafePerformIO)

import Options.Applicative
  ( ArgumentFields
  , Mod
  , OptionFields
  , Parser
  , argument
  , option
  )
import Options.Applicative.Types (ReadM, readerAsk)
import System.OsPath (OsPath, encodeFS)


hyphen :: OsPath
hyphen = unsafePerformIO $ encodeFS "-"
{-# NOINLINE hyphen #-}


period :: OsPath
period = unsafePerformIO $ encodeFS "."
{-# NOINLINE period #-}


path :: ReadM OsPath
path = unsafePerformIO . encodeFS <$> readerAsk


pathOption :: Mod OptionFields OsPath -> Parser OsPath
pathOption = option path


pathArgument :: Mod ArgumentFields OsPath -> Parser OsPath
pathArgument = argument path
