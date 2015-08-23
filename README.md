hackmanager
=====

[![Build Status](https://travis-ci.org/agrafix/hackmanager.svg)](https://travis-ci.org/agrafix/hackmanager)
[![Hackage page (downloads and API reference)][hackage-png]][hackage]

## Intro

Hackage: [hackmanager](http://hackage.haskell.org/package/hackmanager)

Generate useful files for Haskell projects

## Cli Usage: hackmanager

```sh
$ hackmanager --help
hackmanager - Generate useful files for Haskell projects

Usage: hackmanager COMMAND
  Simplify managing Haskell projects by generating files like README.md,
  .travis.yml, etc.

Available options:
  -h,--help                Show this help text

Available commands:
  readme                   
  travis                   
  gitignore                

(c) 2015 Alexander Thiemann - BSD3 License

```

## Library Usage Example

```haskell
module Main where

import Hack.Manager.Collector
import Hack.Manager.Readme

import qualified Data.Text as T

main :: IO ()
main =
    do pi <- getProjectInfo
       case pi of
         Left err -> putStrLn err
         Right info ->
             do rm <- renderReadme info
                putStrLn (T.unpack rm)

```

## Install

* Using cabal: `cabal install `
* From Source (cabal): `git clone https://github.com/agrafix/hackmanager.git && cd hackmanager && cabal install`
* From Source (stack): `git clone https://github.com/agrafix/hackmanager.git && cd hackmanager && stack build`


## Misc

### Supported GHC Versions

* 7.10.2

### License

Released under the BSD3 license.
(c) 2015 Alexander Thiemann
