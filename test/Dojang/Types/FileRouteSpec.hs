{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.FileRouteSpec (monikerMap, paths, spec) where

import Data.HashMap.Strict (HashMap, fromList)
import Data.Text (Text)
import Test.Hspec (Spec, describe, specify)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldNotBe)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.Evaluate (EvaluationWarning (UndefinedMoniker))
import Dojang.Types.Environment (Environment (Environment))
import Dojang.Types.EnvironmentPredicate
  ( EnvironmentPredicate (Moniker, OperatingSystem, Or)
  )
import Dojang.Types.FilePathExpression
  ( FilePathExpression (Root, Substitution)
  )
import Dojang.Types.FileRoute
  ( FileRoute (monikerResolver, predicates)
  , FileType (Directory)
  , dispatch
  , fileRoute
  , fileRoute'
  )
import Dojang.Types.Gen qualified as Gen
import Dojang.Types.MonikerName (MonikerName, parseMonikerName)


moniker :: Text -> MonikerName
moniker name = case parseMonikerName name of
  Right monikerName -> monikerName
  Left _ -> error "Invalid moniker name"


monikerMap :: HashMap MonikerName EnvironmentPredicate
monikerMap =
  fromList
    [ (moniker "linux", OperatingSystem "linux")
    , (moniker "macos", OperatingSystem "macos")
    , (moniker "windows", OperatingSystem "windows")
    ,
      ( moniker "posix"
      , Or [Moniker $ moniker "linux", Moniker $ moniker "macos"]
      )
    ]


paths :: [(MonikerName, Maybe FilePathExpression)]
paths =
  [ (moniker "posix", Just $ Substitution "HOME")
  ,
    ( moniker "windows"
    , Just $ Substitution "USERPROFILE" -- cSpell:disable-line
    )
  ]


spec :: Spec
spec = do
  let route = fileRoute monikerMap paths Directory
  describe "FileRoute" $ do
    specify "monikerResolver" $ do
      route.monikerResolver (moniker "linux")
        `shouldBe` Just (OperatingSystem "linux")
      route.monikerResolver (moniker "non-existent") `shouldBe` Nothing

    describe "Eq" $ do
      specify "properties" $ hedgehog $ do
        x <- forAll Gen.fileRoute
        y <- forAll Gen.fileRoute
        (x == x) === True
        (x == y) === (y == x)
        (x /= y) === (y /= x)

      specify "examples" $ do
        route `shouldBe` route
        route
          `shouldBe` fileRoute
            monikerMap
            [ (moniker "posix", Just $ Substitution "HOME")
            ,
              ( moniker "windows"
              , Just $ Substitution "USERPROFILE" -- cSpell:disable-line
              )
            ]
            Directory
        route
          `shouldNotBe` fileRoute
            monikerMap
            [ (moniker "posix", Just $ Substitution "HOME")
            , (moniker "windows", Just $ Substitution "HOME")
            ]
            Directory
        route
          `shouldNotBe` fileRoute'
            (const Nothing)
            [ (Moniker $ moniker "posix", Just $ Substitution "HOME")
            ,
              ( Moniker $ moniker "windows"
              , Just $ Substitution "USERPROFILE" -- cSpell:disable-line
              )
            ]
            Directory

    specify "predicates" $ do
      route.predicates
        `shouldBe` ( [
                       ( Moniker $ moniker "windows"
                       , Just $ Substitution "USERPROFILE" -- cSpell:disable-line
                       )
                     , (Moniker $ moniker "posix", Just $ Substitution "HOME")
                     ]
                      :: [(EnvironmentPredicate, Maybe FilePathExpression)]
                   )

    specify "Show" $ do
      show route
        `shouldBe` ("FileRoute " ++ show (route.predicates) ++ " Directory")
      showList [route] ""
        `shouldBe` ("[FileRoute " ++ show (route.predicates) ++ " Directory]")

  specify "dispatch" $ do
    dispatch (Environment "linux" "x86_64") route
      `shouldBe` ([Just $ Substitution "HOME"], [])
    dispatch (Environment "macos" "aarch64") route
      `shouldBe` ([Just $ Substitution "HOME"], [])
    dispatch (Environment "windows" "x86_64") route
      `shouldBe` ([Just $ Substitution "USERPROFILE"], []) -- cSpell:disable-line
    dispatch (Environment "windows" "x86_64") route
      `shouldBe` ([Just $ Substitution "USERPROFILE"], []) -- cSpell:disable-line
    let withWarnings =
          fileRoute
            monikerMap
            (paths ++ [(moniker "non-existent", Just $ Root Nothing)])
            Directory
    dispatch (Environment "linux" "x86_64") withWarnings
      `shouldBe` ( [Just $ Substitution "HOME"]
                 , [UndefinedMoniker (moniker "non-existent")]
                 )
