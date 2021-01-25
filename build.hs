#!/usr/bin/env stack
-- stack --resolver lts-17.0 script --package shake --package bytestring --package cereal --package hspec --package process


-- initial version taken from https://shakebuild.com/manual


import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
  want ["_build/vm" <.> exe]

  phony "clean" $ do
    putInfo "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]
    removeFilesAfter "zig-cache" ["//*"]

  phony "run" $ do
    need ["_build/vm" <.> exe]
    putInfo "Running vm"
    cmd_ "_build/vm" "a.bin"

  phony "test" $ do
    need ["_build/vm" <.> exe]
    putInfo "Testing vm"
    cmd_ "tests/Tests.hs" ""

  phony "valgrind" $ do
    need ["_build/vm-valgrind" <.> exe]
    putInfo "Running vm-valgrind"
    cmd_ "valgrind --leak-check=yes" "_build/vm-valgrind a.bin"

  "_build/vm" <.> exe %> \out -> do
    cs <- getDirectoryFiles "src" ["//*.c"]
    let os = ["_build" </> "normal" </> "src" </> c -<.> "o" | c <- cs]
    need os
    cmd_ "zig cc -o" [out] os

  "_build/vm-valgrind" <.> exe %> \out -> do
    cs <- getDirectoryFiles "src" ["//*.c"]
    let os = ["_build" </> "valgrind" </> "src" </> c -<.> "o" | c <- cs]
    need os
    cmd_ "zig cc -g -o" [out] os

  "_build/normal//*.o" %> \out -> do
    let c = dropDirectory1 $ dropDirectory1 $ out -<.> "c"
    let m = out -<.> "m"
    cmd_ "zig cc -c" [c] "-o" [out] "-MMD -MF" [m]
    neededMakefileDependencies m

  "_build/valgrind//*.o" %> \out -> do
    let c = dropDirectory1 $ dropDirectory1 $ out -<.> "c"
    let m = out -<.> "m"
    cmd_ "zig cc -g -c" [c] "-o" [out] "-MMD -MF" [m]
    neededMakefileDependencies m
