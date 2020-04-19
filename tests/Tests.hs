#!/usr/bin/env stack
-- stack --resolver lts-15.7 script --package bytestring --package cereal --package hspec --package process

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Bits
import Data.Int
import Data.Serialize
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Test.Hspec
import System.Process


main :: IO ()
main = hspec $ do
  describe "tests" $ do
    it "prints 17" $ do
      writeTestToFile "a.bin" . compile $
        [ Lit 17, Print, Halt ]
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "17\n"

    it "print (1 + 2)" $ do
      writeTestToFile "a.bin" . compile $
        [ Lit 1, Lit 2, Add, Print, Halt ]
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "3\n"

    it "print (1 + (2 + (3 + (4 + 5))))" $ do
      writeTestToFile "a.bin" . compile $
        [ Lit 1, Lit 2, Lit 3, Lit 4, Lit 5, Add, Add, Add, Add, Print, Halt ]
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "15\n"

    it "print (swap 1 2)" $ do
      writeTestToFile "a.bin" . compile $
        [ Lit 1, Lit 2, Swap, Print, Halt ]
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "1\n"

    it "prints \"hello\"" $ do
      writeTestToFile "a.bin" . compile $
        [ LitStr "hello", PrintStr, Halt ]
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "hello\n"

    it "prints \"hello\" \"world\"" $ do
      writeTestToFile "a.bin" . compile $
        [ LitStr "hello", PrintStr, Pop
        , LitStr "world", PrintStr, Pop
        , Halt
        ]
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "hello\nworld\n"

    it "prints a few lines" $ do
      writeTestToFile "a.bin" . compile $
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
      writeTestToFile "a.bin" . compile $
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

      writeTestToFile "a.bin" . compile $ code
      result <- readProcess "_build/vm" ["a.bin"] ""
      result `shouldBe` "b\n"

type Program' = [Stmt]

data Stmt
  = Lit Int64
  | LitStr BSC.ByteString
  | Add
  | Swap
  | Pop
  | Print
  | PrintStr
  | Halt
  deriving (Show, Read)

halt_  = putWord8 0
load_  = putWord8 1
print_ = putWord8 2
swap_  = putWord8 3
pop_   = putWord8 4
add_   = putWord8 5
lit_ n = putInt64le (shiftL n 1 + 1)
str_ :: BS.ByteString -> Put
str_ str = do
  putWord8 6
  putWord16le (fromIntegral (setBit (shiftL (BSC.length str) 2) 1))
  mapM_ (putInt8 . fromIntegral) (BS.unpack str)
printStr_ = putWord8 7

compileStmt :: Stmt -> Put
compileStmt = \case
  Lit n -> load_ *> lit_ n
  LitStr str -> str_ str
  Add -> sequence_ [add_, swap_, pop_, swap_, pop_]
  Swap -> swap_
  Pop -> pop_
  Print -> print_
  PrintStr -> printStr_
  Halt -> halt_

compile :: Program' -> Put
compile = mapM_ compileStmt

writeTestToFile :: FilePath -> Put -> IO ()
writeTestToFile file =
  BS.writeFile file . runPut

