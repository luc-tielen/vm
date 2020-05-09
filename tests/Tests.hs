#!/usr/bin/env stack
-- stack --resolver lts-15.7 script --package bytestring --package cereal --package hspec --package process

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BSC
import Test.Hspec
import System.Process

import Codegen


main :: IO ()
main = hspec $ do
  describe "tests" $ do
    it "prints 17" $ do
      writeBytesToFile "a.bin" . compile $
        [ Lit 17, Print, Halt ]
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "17\n"

    it "print (1 + 2)" $ do
      writeBytesToFile "a.bin" . compile $
        [ Lit 1, Lit 2, Add, Print, Halt ]
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "3\n"

    it "print (1 + (2 + (3 + (4 + 5))))" $ do
      writeBytesToFile "a.bin" . compile $
        [ Lit 1, Lit 2, Lit 3, Lit 4, Lit 5, Add, Add, Add, Add, Print, Halt ]
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "15\n"

    it "print (swap 1 2)" $ do
      writeBytesToFile "a.bin" . compile $
        [ Lit 1, Lit 2, Swap, Print, Halt ]
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "1\n"

    it "prints \"hello\"" $ do
      writeBytesToFile "a.bin" . compile $
        [ LitStr "hello", PrintStr, Halt ]
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "hello\n"

    it "prints \"hello\" \"world\"" $ do
      writeBytesToFile "a.bin" . compile $
        [ LitStr "hello", PrintStr, Pop
        , LitStr "world", PrintStr, Pop
        , Halt
        ]
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "hello\nworld\n"

    it "prints a few lines" $ do
      writeBytesToFile "a.bin" . compile $
        [ LitStr "hello",  PrintStr
        , LitStr "world",  PrintStr, Pop
        , LitStr "how",    PrintStr, Pop
        , LitStr "are",    PrintStr
        , LitStr "you",    PrintStr, Pop
        , LitStr "today?", PrintStr, Pop
        , Halt
        ]
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "hello\nworld\nhow\nare\nyou\ntoday?\n"

    it "prints a few more lines" $ do
      writeBytesToFile "a.bin" . compile $
        [ LitStr "hello",       PrintStr
        , LitStr "world",       PrintStr, Pop
        , LitStr "how",         PrintStr, Pop
        , LitStr "are",         PrintStr
        , LitStr "you",         PrintStr, Pop
        , LitStr "today?",      PrintStr, Pop
        , LitStr "let",         PrintStr
        , LitStr "me",          PrintStr, Pop
        , LitStr "tell",        PrintStr, Pop
        , LitStr "you",         PrintStr
        , LitStr "something",   PrintStr, Pop
        , LitStr "interesting", PrintStr, Pop
        , LitStr "about",       PrintStr, Pop
        , LitStr "fish?",       PrintStr, Pop
        , LitStr "i dunno",     PrintStr
        , LitStr "have ",       PrintStr, Pop
        , LitStr " fun",        PrintStr, Pop
        , Halt
        ]
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` unlines
        [ "hello"
        , "world"
        , "how"
        , "are"
        , "you"
        , "today?"
        , "let"
        , "me"
        , "tell"
        , "you"
        , "something"
        , "interesting"
        , "about"
        , "fish?"
        , "i dunno"
        , "have "
        , " fun"
        ]

  -- We want to test the gen1 gc
  -- It currently has the size of 32
  -- We want to load many literals to trigger the gen0 gc
  -- But we want to get rid of them at some point
    it "gen0 + gen1 gc" $ do
      let
        strings = (<>)
          [ LitStr (BSC.pack [c]) | c <- ['a'..'e'] ]
          [ Swap, Pop, Swap, Pop, Swap, Pop ]
        code = take 120 (cycle strings) <> [PrintStr, Halt]

      writeBytesToFile "a.bin" . compile $ code
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "b\n"

    it "simple list" $ do
      let
        code = [Lit 1, Lit 2, Nil, Cons 2, Cons 2, HeapIndex 0, Print, Halt] -- (1 : 2 : Nil)

      writeBytesToFile "a.bin" . compile $ code
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "1\n"


    it "simple list 2" $ do
      let
        code =
          [ Lit 1, Lit 2, Nil, Cons 2, Cons 2  -- (1 : 2 : Nil)
          , HeapIndex 0, Print, Pop
          , HeapIndex 1, HeapIndex 0, Print, Pop, Pop
          , Halt
          ]

      writeBytesToFile "a.bin" . compile $ code
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "1\n2\n"


    it "An array" $ do
      let
        code = concat
          [ map Lit [0..9]
          , [ Cons 10
            , HeapIndex 7, Print
            , Halt
            ]
          ]

      writeBytesToFile "a.bin" . compile $ code
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "7\n"


    it "A 2d array" $ do
      let
        size = 10
        index = 2
        code = concat
          [ concatMap (\n -> [Lit n, Lit n, Lit n, Cons 3]) [0..(size - 1)]
          , [ Cons (fromIntegral size)
            , HeapIndex index, HeapIndex 1, Print
            , Halt
            ]
          ]

      writeBytesToFile "a.bin" . compile $ code
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` (show index <> "\n")

    it "A 2d array throwaway" $ do
      let
        size = 10
        index = 2
        code = concat
          [ concatMap (\n -> [Lit n, Lit n, Lit n, Cons 3]) [0..(size - 1)]
          , [ Cons (fromIntegral size)
            , Pop
            ]
          , concatMap (\n -> [Lit n, Lit n, Lit n, Cons 3]) [0..(size - 1)]
          , [ Cons (fromIntegral size)
            , HeapIndex index, HeapIndex 1, Print
            , Halt
            ]
          ]

      writeBytesToFile "a.bin" . compile $ code
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` (show index <> "\n")
