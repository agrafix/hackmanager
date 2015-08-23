{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hack.Manager.Collector
import Hack.Manager.Readme
import Hack.Manager.Travis
import Hack.Manager.Gitignore
import Hack.Manager.Types

import Control.Monad
import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = join $ execParser optParser

withProjectInfo :: (ProjectInfo -> IO ()) -> IO ()
withProjectInfo cont =
    do raw <- getProjectInfo
       case raw of
         Left err ->
             putStrLn err
         Right pinfo ->
             cont pinfo

makeReadme :: IO ()
makeReadme =
    withProjectInfo $ \pinfo ->
    do rm <- renderReadme pinfo
       T.writeFile "README.md" rm

makeTravis :: TravisOpts -> IO ()
makeTravis to =
    withProjectInfo $ \pinfo ->
    do travis <- renderTravis to pinfo
       T.writeFile ".travis.yml" travis

travisOptsParser :: Parser TravisOpts
travisOptsParser =
    TravisOpts
    <$> (T.pack <$> argument str (metavar "GHC-RELEASE_VERSION"))
    <*> switch (long "use-stack" <> help "Use stack to run travis build")

makeGitignore :: IO ()
makeGitignore =
    withProjectInfo $ \pinfo ->
    do gitignore <- renderGitignore pinfo
       T.writeFile ".gitignore" gitignore

commands :: Mod CommandFields (IO ())
commands =
    command "readme" (info (pure makeReadme) idm)
    <> command "travis" (info (makeTravis <$> travisOptsParser) idm)
    <> command "gitignore" (info (pure makeGitignore) idm)

optParser :: ParserInfo (IO ())
optParser =
    info (helper <*> subparser commands)
         ( fullDesc
         <> progDesc "Simplify managing Haskell projects by generating files like README.md, .travis.yml, etc."
         <> header "hackmanager - Generate useful files for Haskell projects"
         <> footer "(c) 2015 Alexander Thiemann - BSD3 License"
         )
