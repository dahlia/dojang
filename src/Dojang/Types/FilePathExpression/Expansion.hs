module Dojang.Types.FilePathExpression.Expansion
  ( ExpansionWarning (..)
  , expandFilePath
  ) where

import Dojang.Types.FilePathExpression
  ( EnvironmentVariable
  , FilePathExpression (..)
  )

import Data.Text (Text)
import System.OsPath (OsPath, OsString, pack, pathSeparator, unsafeFromChar, (</>))


-- | A set of warnings that can occur during expansion.
newtype ExpansionWarning
  = -- | The case where an environment variable that is referenced in a
    -- 'Substitution' is not defined.
    UndefinedEnvironmentVariable EnvironmentVariable
  deriving (Eq, Show)


-- | Expands a 'FilePathExpression' into an 'OsPath'.
expandFilePath
  :: (Monad m)
  => FilePathExpression
  -- ^ The 'FilePathExpression' to expand.
  -> (EnvironmentVariable -> m (Maybe OsString))
  -- ^ A function that can look up an environment variable.
  -> (Text -> m OsString)
  -- ^ A function that encodes a 'Text' into an 'OsString'.
  -> m (OsPath, [ExpansionWarning])
  -- ^ The expanded 'OsPath', along with a list of warnings that occurred during
  -- expansion (if any).
expandFilePath (BareComponent component) _ encode = do
  f <- encode component
  return (f, [])
expandFilePath (Root Nothing) _ _ = return (pack [pathSeparator], [])
expandFilePath (Root (Just driveLetter)) _ _ =
  return
    ( pack
        [ unsafeFromChar driveLetter
        , unsafeFromChar ':'
        , pathSeparator
        ]
    , []
    )
expandFilePath (Concatenation expr1 expr2) lookupEnv encode = do
  (expanded1, ws1) <- expandFilePath expr1 lookupEnv encode
  (expanded2, ws2) <- expandFilePath expr2 lookupEnv encode
  return (expanded1 <> expanded2, ws1 ++ ws2)
expandFilePath (PathSeparator expr1 expr2) lookupEnv encode = do
  (expanded1, ws1) <- expandFilePath expr1 lookupEnv encode
  (expanded2, ws2) <- expandFilePath expr2 lookupEnv encode
  return (expanded1 </> expanded2, ws1 ++ ws2)
expandFilePath (Substitution envVar) lookupEnv _ = do
  v <- lookupEnv envVar
  case v of
    Just v' -> return (v', [])
    Nothing -> return (mempty, [UndefinedEnvironmentVariable envVar])
expandFilePath (SubstitutionWithDefault envVar expr) lookupEnv encode = do
  env <- lookupEnv envVar
  case env of
    Just env' | env' /= mempty -> return (env', [])
    _ -> expandFilePath expr lookupEnv encode
expandFilePath (ConditionalSubstitution envVar expr) lookupEnv encode = do
  env <- lookupEnv envVar
  case env of
    Just env' | env' /= mempty -> expandFilePath expr lookupEnv encode
    _ -> return (mempty, [])
