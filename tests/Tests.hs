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
      writeTestToFile "a.bin" . compile $ [ Lit 17, Print, Halt ]
      callProcess "_build/vm" ["a.bin"]

    it "print (1 + 2)" $ do
      writeTestToFile "a.bin" . compile $ [ Lit 1, Lit 2, Add, Print, Halt ]
      callProcess "_build/vm" ["a.bin"]

    it "print (1 + (2 + (3 + (4 + 5))))" $ do
      writeTestToFile "a.bin" . compile $ [ Lit 1, Lit 2, Lit 3, Lit 4, Lit 5, Add, Add, Add, Add, Print, Halt ]
      callProcess "_build/vm" ["a.bin"]

    it "print (swap 1 2)" $ do
      writeTestToFile "a.bin" . compile $ [ Lit 1, Lit 2, Swap, Print, Halt ]
      callProcess "_build/vm" ["a.bin"]

type Program = [Int32]
type Program' = [Stmt]

data Stmt
  = Lit Int32
  | Add
  | Swap
  | Print
  | Halt
  deriving (Show, Read)

halt_  = 0
load_  = 1
print_ = 2
swap_  = 3
pop_   = 4
add_   = 5

compileStmt :: Stmt -> Program
compileStmt = \case
  Lit n -> [load_, n]
  Add -> [add_, swap_, pop_, swap_, pop_]
  Swap -> [swap_]
  Print -> [print_]
  Halt -> [halt_]

compile :: Program' -> Program
compile = concatMap compileStmt

writeTestToFile :: FilePath -> Program -> IO ()
writeTestToFile file =
  BS.writeFile file . runPut . mapM_ putInt32le

