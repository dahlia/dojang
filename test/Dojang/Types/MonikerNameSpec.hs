{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Dojang.Types.MonikerNameSpec (spec) where

import Dojang.Types.Gen
  ( emptyMonikerNameText
  , invalidMonikerNameText
  , monikerName
  , monikerNameError
  , monikerNameText
  , monikerNameTextHavingInvalidCharacterWithIndex
  , monikerNameTextStartingWithNonLetter
  , monikerNameWithCIText
  )
import Dojang.Types.MonikerName
  ( MonikerNameError (Empty, HavingInvalidCharacter, StartingWithNonLetter)
  , parseMonikerName
  )

import Data.CaseInsensitive (foldCase, mk)
import Data.Either (isLeft)
import Data.Hashable (Hashable (hashWithSalt))
import Test.Hspec (Spec, describe, specify)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.Hedgehog
  ( assert
  , forAll
  , hedgehog
  , (===)
  )


spec :: Spec
spec = do
  describe "parseMonikerName" $ do
    specify "examples" $ do
      let Right a = parseMonikerName "foo-bar"
      a.name `shouldBe` "foo-bar"
      let Right b = parseMonikerName "甲乙_丙丁"
      b.name `shouldBe` "甲乙_丙丁"
      parseMonikerName "" `shouldBe` Left Empty
      parseMonikerName "1" `shouldBe` Left StartingWithNonLetter
      parseMonikerName "*" `shouldBe` Left StartingWithNonLetter
      parseMonikerName "$foo" `shouldBe` Left StartingWithNonLetter
      parseMonikerName "foo\tbar" `shouldBe` Left (HavingInvalidCharacter 3)

    specify "properties" $ hedgehog $ do
      forAll monikerNameText >>= \name ->
        let Right parsed = parseMonikerName name
        in parsed.name === mk name
      forAll invalidMonikerNameText >>= \name ->
        assert $ isLeft $ parseMonikerName name
      forAll emptyMonikerNameText >>= \name ->
        parseMonikerName name === Left Empty
      forAll monikerNameTextStartingWithNonLetter >>= \name ->
        parseMonikerName name === Left StartingWithNonLetter
      forAll monikerNameTextHavingInvalidCharacterWithIndex >>= \(name, idx) ->
        parseMonikerName name === Left (HavingInvalidCharacter idx)

  describe "MonikerName" $ do
    specify "Eq" $ hedgehog $ do
      name <- forAll monikerName
      name' <- forAll monikerName
      (name == name') === (foldCase name.name == foldCase name'.name)
      (name /= name') === (foldCase name.name /= foldCase name'.name)

    specify "Ord" $ hedgehog $ do
      name <- forAll monikerName
      name' <- forAll monikerName
      let foldedName = foldCase name.name
      let foldedName' = foldCase name'.name
      name `compare` name' === foldedName `compare` foldedName'
      (name < name') === (foldedName < foldedName')
      (name <= name') === (foldedName <= foldedName')
      (name > name') === (foldedName > foldedName')
      (name >= name') === (foldedName >= foldedName')
      (min name name').name === min foldedName foldedName'
      (max name name').name === max foldedName foldedName'

    specify "Hashable" $ hedgehog $ do
      (name, nameText) <- forAll monikerNameWithCIText
      hashWithSalt 0 name === hashWithSalt 0 (foldCase nameText)

    specify "HasField \"name\"" $ hedgehog $ do
      (name, nameText) <- forAll monikerNameWithCIText
      name.name === nameText

    specify "Show" $ hedgehog $ do
      (name, nameText) <- forAll monikerNameWithCIText
      show name === "MonikerName " ++ show nameText
      shows name "" === "MonikerName " ++ shows nameText ""
      showList [name] "" === "[MonikerName " ++ show nameText ++ "]"

  describe "MonikerNameError" $ do
    specify "Eq" $ hedgehog $ do
      e <- forAll monikerNameError
      e' <- forAll monikerNameError
      (e == e') === (show e == show e')
      (e /= e') === (show e /= show e')

    specify "Show" $ hedgehog $ do
      e <- forAll monikerNameError
      read (show e) === e
      read (shows e "") === e
      read (showList [e] "") === [e]
