![Build status](https://gitlab.com/gilmi/vm/badges/master/pipeline.svg?job=pipeline)

# Bytecode Interpreter Project

Hi! I'm streaming work on this project on [Twitch](https://twitch.tv/suppipi)!
The entire series of videos can be viewed on [Youtube](https://www.youtube.com/watch?v=pKsOAJzfLgE&list=PLhMOOgDOKD4JV2AgL1XzoYanY6RpGKbNa) as well.

## Overview

BIP (name pending) is stack-based bytecode interpreter (/virtual machine)
for statically typed functional languages written in C.

I hope to stick with this project for long enough to get to implement closures,
garbage collection, exceptions and concurrency.
I'll be taking cues from OCaml and Chicken Scheme
as well as other resources, combined with some experimentation.

- [Check out the design doc](design.org).

## How to build

#### Prerequisites

You will need to install [Stack](https://haskellstack.org) and [Zig](https://ziglang.org).

#### Build and Run

Build using `./build.hs`. Stack will download GHC (A Haskell compiler) and several packages the first time you run this so this might take some time.

After that:

- `./build.hs test` - To run tests
- `./build.hs run` - To interpret and run the file `a.bin`
- `./build.hs valgrind` - To interpret and run the file `a.bin` using valgrind
- `./build.hs clean` - To clean

## Motivation

I'm using this project to brush up on my C, learn more about runtime systems,
and potentially use this as a backend to my WIP functional language.
I'm not entirely sure what I'm doing, but I'll figure it out!

## Progress

Check out the [todo.org](/todo.org) file for the current status.
Note that this project is built
[incrementally](https://gilmi.me/blog/post/2020/04/06/my-side-projects-workflow)
and in short sessions.
Sometimes I'll revise my design decisions, refactor my work,
and change the priority of some tasks. It's all part of the plan!

## Resources

Want to learn more about similar topics? [Take a look at this list of resources](https://github.com/soupi/rfc/blob/master/fun-compilers.md).
