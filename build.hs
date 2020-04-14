#!/usr/bin/env stack
-- stack --resolver lts-15.7 script


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
    putInfo "Running _build/vm"
    cmd_ "_build/vm" "_build/vm"

  phony "test" $ do
    need ["_build/vm" <.> exe]
    putInfo "Testing vm"
    cmd_ "tests/Tests.hs" ""

  "_build/vm" <.> exe %> \out -> do
    cs <- getDirectoryFiles "src" ["//*.c"]
    let os = ["_build" </> "src" </> c -<.> "o" | c <- cs]
    need os
    cmd_ "zig cc -o" [out] os

  "_build//*.o" %> \out -> do
    let c = dropDirectory1 $ out -<.> "c"
    let m = out -<.> "m"
    cmd_ "zig cc -c" [c] "-o" [out] "-MMD -MF" [m]
    neededMakefileDependencies m
