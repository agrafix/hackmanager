hackmanager
=====

[![Build Status](https://travis-ci.org/agrafix/hackmanager.svg)](https://travis-ci.org/agrafix/hackmanager)


## Intro


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

* From Source (cabal): `git clone https://github.com/agrafix/hackmanager.git && cd hackmanager && cabal install`
* From Source (stack): `git clone https://github.com/agrafix/hackmanager.git && cd hackmanager && stack build`

## Features

* Automagically collect package information such as
	* package name
	* GHC compatibility
	* stack Project
	* Hackage / Stackage status
	* License
	* Examples
	* Cli Usage
* Typecheck examples
* Generate informative README.md (Can be extended using a MORE.md)
* Generate .travis.yml (cabal or stack based)
* Generate .gitignore

The generated `.travis.yml` and `.gitignore` are intended as starting templates, while the generated `README.md` should not be modified by hand. Rerun `hackmanager readme` before every commit (commit hook?) to keep it up to date. If you would like to add custom sections, create a `MORE.md`.

## Roadmap

There's no real roadmap - I will add features as needed. I am open to any contributions!

## Misc

### Supported GHC Versions

* 7.10.2

### License

Released under the BSD3 license.
(c) 2015 Alexander Thiemann
