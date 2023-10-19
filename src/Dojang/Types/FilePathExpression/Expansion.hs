module Dojang.Types.FilePathExpression.Expansion (expandFilePath) where

import Dojang.Types.FilePathExpression
  ( EnvironmentVariable
  , FilePathExpression (..)
  )

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.OsPath (OsPath, pack, pathSeparator, unsafeFromChar, (</>))
import System.OsString (OsString)


-- | Expands a 'FilePathExpression' into an 'OsPath'.
expandFilePath
  :: (Monad m)
  => FilePathExpression
  -- ^ The 'FilePathExpression' to expand.
  -> (EnvironmentVariable -> m (Maybe OsString))
  -- ^ A function that can look up an environment variable.
  -> (Text -> m OsString)
  -- ^ A function that encodes a 'Text' into an 'OsString'.
  -> m OsPath
  -- ^ The expanded 'OsPath'.
expandFilePath (BareComponent component) _ encode = encode component
expandFilePath (Root Nothing) _ _ = return $ pack [pathSeparator]
expandFilePath (Root (Just driveLetter)) _ _ =
  return
    $ pack
      [ unsafeFromChar driveLetter
      , unsafeFromChar ':'
      , pathSeparator
      ]
expandFilePath (Concatenation expr1 expr2) lookupEnv encode = do
  expanded1 <- expandFilePath expr1 lookupEnv encode
  expanded2 <- expandFilePath expr2 lookupEnv encode
  return $ expanded1 <> expanded2
expandFilePath (PathSeparator expr1 expr2) lookupEnv encode = do
  expanded1 <- expandFilePath expr1 lookupEnv encode
  expanded2 <- expandFilePath expr2 lookupEnv encode
  return $ expanded1 </> expanded2
expandFilePath (Substitution envVar) lookupEnv _ =
  fromMaybe mempty <$> lookupEnv envVar
expandFilePath (SubstitutionWithDefault envVar expr) lookupEnv encode = do
  env <- lookupEnv envVar
  case env of
    Just env' | env' /= mempty -> return env'
    _ -> expandFilePath expr lookupEnv encode
expandFilePath (ConditionalSubstitution envVar expr) lookupEnv encode = do
  env <- lookupEnv envVar
  case env of
    Just env' | env' /= mempty -> expandFilePath expr lookupEnv encode
    _ -> return mempty
