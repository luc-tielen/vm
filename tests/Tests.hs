#!/usr/bin/env stack
-- stack --resolver lts-15.7 script --package bytestring --package cereal --package hspec --package process

{-# LANGUAGE LambdaCase #-}

import Data.Int
import Data.Serialize
import qualified Data.ByteString as BS
import System.Process

import Test.Hspec

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

type Program' = [Stmt]

data Stmt
  = Lit Int64
  | Add
  | Swap
  | Print
  | Halt
  deriving (Show, Read)

halt_  = putWord8 0
load_  = putWord8 1
print_ = putWord8 2
swap_  = putWord8 3
pop_   = putWord8 4
add_   = putWord8 5
lit_ n = putInt64le n

compileStmt :: Stmt -> Put
compileStmt = \case
  Lit n -> load_ *> lit_ n
  Add -> sequence_ [add_, swap_, pop_, swap_, pop_]
  Swap -> swap_
  Print -> print_
  Halt -> halt_

compile :: Program' -> Put
compile = mapM_ compileStmt

writeTestToFile :: FilePath -> Put -> IO ()
writeTestToFile file =
  BS.writeFile file . runPut

