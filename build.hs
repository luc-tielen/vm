#!/usr/bin/env stack
-- stack --resolver lts-17.0 script --package shake --package bytestring --package cereal --package hspec --package process


-- initial version taken from https://shakebuild.com/manual


import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="zig-out"} $ do
  want ["zig-out/bin/vm"]

  phony "clean" $ do
    putInfo "Cleaning files"
    removeFilesAfter "zig-out" ["//*"]
    removeFilesAfter "zig-cache" ["//*"]

  phony "run" $ do
    need ["zig-out/bin/vm"]
    putInfo "Running vm"
    cmd_ "zig-out/bin/vm" "a.bin"

  phony "test" $ do
    need ["zig-out/bin/vm"]
    putInfo "Testing vm"
    cmd_ "tests/Tests.hs" ""

  phony "valgrind" $ do
    need ["zig-out/bin/vm-valgrind"]
    putInfo "Running vm-valgrind"
    cmd_ "valgrind --leak-check=yes" "zig-out/bin/vm-valgrind a.bin"

  "zig-out/bin/vm" %> \_ -> do
    alwaysRerun
    cmd_ "zig build" -- -Drelease-safe=true"

  "zig-out/bin/vm-valgrind" %> \out -> do
    alwaysRerun
    cmd_ "zig build"
    copyFileChanged "zig-out/bin/vm" "zig-out/bin/vm-valgrind"
