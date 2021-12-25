#!/usr/bin/env stack
-- stack --resolver lts-17.0 script --package bytestring --package cereal --package hspec --package process

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
      compareResult
        [ Lit 17, Print, Halt ]
        [ "17" ]

    it "print (1 + 2)" $ do
      compareResult
        [ Lit 1, Lit 2, Add, Print, Halt ]
        [ "3" ]

    it "print (1 + (2 + (3 + (4 + 5))))" $ do
      compareResult
        [ Lit 1, Lit 2, Lit 3, Lit 4, Lit 5, Add, Add, Add, Add, Print, Halt ]
        [ "15" ]

    it "print (swap 1 2)" $ do
      compareResult
        [ Lit 1, Lit 2, Swap, Print, Halt ]
        [ "1" ]

    it "prints \"hello\"" $ do
      compareResult
        [ LitStr "hello", PrintStr, Halt ]
        [ "hello" ]

    it "prints \"hello\" \"world\"" $ do
      let
        code =
          [ LitStr "hello", PrintStr, Pop
          , LitStr "world", PrintStr, Pop
          , Halt
          ]
        expectedResult =
          [ "hello"
          , "world"
          ]
      code `compareResult` expectedResult

    it "prints a few lines" $ do
      let
        code =
          [ LitStr "hello",  PrintStr
          , LitStr "world",  PrintStr, Pop
          , LitStr "how",    PrintStr, Pop
          , LitStr "are",    PrintStr
          , LitStr "you",    PrintStr, Pop
          , LitStr "today?", PrintStr, Pop
          , Halt
          ]
        expectedResult =
          [ "hello"
          , "world"
          , "how"
          , "are"
          , "you"
          , "today?"
          ]
      code `compareResult` expectedResult

    it "prints a few more lines" $ do
      let
        code =
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
        expectedResult =
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
      code `compareResult` expectedResult

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

      code `compareResult` ["b"]

    it "simple list" $ do
      let
        code = [Lit 1, Lit 2, Nil, Cons 2, Cons 2, HeapIndex 0, Print, Halt] -- (1 : 2 : Nil)

      writeBytesToFile "a.bin" . compile $ code
      result <- readProcess "zig-out/bin/vm" ["a.bin"] ""
      code `compareResult` ["1"]


    it "simple list 2" $ do
      let
        code =
          [ Lit 1, Lit 2, Nil, Cons 2, Cons 2  -- (1 : 2 : Nil)
          , HeapIndex 0, Print, Pop
          , HeapIndex 1, HeapIndex 0, Print, Pop, Pop
          , Halt
          ]

      code `compareResult` ["1", "2"]


    it "An array" $ do
      let
        code = concat
          [ map Lit [0..9]
          , [ Cons 10
            , HeapIndex 7, Print
            , Halt
            ]
          ]

      code `compareResult` ["7"]


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

      code `compareResult` [show index]

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

      code `compareResult` [show index]

    it "A string array throwaway" $ do
      let
        size = 5
        index = 0
        allocArrayCode = concat
          [ concatMap
            (\_ ->
               [LitStr "hello", LitStr "world", LitStr "aaaaaaaaa", LitStr "bye", LitStr "now", Cons 5]
            )
            [0..(size - 1)]
          , [ Cons (fromIntegral size)
            ]
          ]
        code = concat
          [ allocArrayCode
          , [ HeapIndex index, HeapIndex 1
            , Swap, Pop, Swap, Pop
            ]
          , allocArrayCode
          , [ Swap
            , PrintStr
            , Halt
            ]
          ]

      code `compareResult` ["world"]




compareResult :: Program -> [String] -> IO ()
compareResult code shouldbe = do
  writeBytesToFile "a.bin" . compile $ code
  result <- readProcess "zig-out/bin/vm" ["a.bin"] ""
  result `shouldBe` (unlines shouldbe)
