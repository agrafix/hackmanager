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
