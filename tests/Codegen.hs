{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import Data.Word
import Data.Bits
import Data.Int
import Data.Serialize
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

-----------
-- Types --
-----------


type Program = [Stmt]

data Stmt
  = Lit Int64
  | LitStr BSC.ByteString
  | Add
  | Swap
  | Pop
  | Print
  | PrintStr
  | Cons Word16
  | Nil
  | HeapIndex Word16
  | Halt
  deriving (Show, Read)

-------------
-- Compile --
-------------

compile :: Program -> Put
compile = mapM_ compileStmt

writeBytesToFile :: FilePath -> Put -> IO ()
writeBytesToFile file =
  BS.writeFile file . runPut

---

compileStmt :: Stmt -> Put
compileStmt = \case
  Lit n -> load_ *> lit_ n
  LitStr str -> str_ str
  Add -> sequence_ [add_, swap_, pop_, swap_, pop_]
  Swap -> swap_
  Pop -> pop_
  Print -> print_
  PrintStr -> printStr_
  Cons size ->
    sequence_
      ( cons_ size
      : take (fromIntegral size * 2) (cycle [swap_, pop_])
      )
  Nil -> compileStmt (Lit 0)
  HeapIndex i -> index_ i
  Halt -> halt_

---------------
-- Serialize --
---------------

halt_  :: Put
halt_  = putWord8 0

load_ :: Put
load_  = putWord8 1

print_ :: Put
print_ = putWord8 2

swap_ :: Put
swap_ = putWord8 3

pop_ :: Put
pop_ = putWord8 4

add_ :: Put
add_ = putWord8 5

lit_ :: Int64 -> Put
lit_ n = putInt64le (markIntLit (shiftL n 1))

str_ :: BS.ByteString -> Put
str_ str = do
  putWord8 6
  putWord16le (fromIntegral (markByteArray (shiftL (BSC.length str) 2)))
  mapM_ (putInt8 . fromIntegral) (BS.unpack str)

printStr_ :: Put
printStr_ = putWord8 7

cons_ :: Word16 -> Put
cons_ size = do
  putWord8 8
  putWord16le (fromIntegral (shiftL size 2))

index_ :: Word16 -> Put
index_ i = do
  putWord8 9
  putWord16le (fromIntegral (shiftL i 2))

---

markByteArray :: Int -> Int
markByteArray num = setBit num 1

markIntLit :: Int64 -> Int64
markIntLit = (+1)
