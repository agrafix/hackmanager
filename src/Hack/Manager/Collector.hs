{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Hack.Manager.Collector where

import Hack.Manager.Types

import Control.Exception
import Control.Monad.Except
import System.Directory
import System.Exit
import System.Process
import qualified Data.List as L
import qualified Data.Text as T
import qualified Distribution.Compiler as Comp
import qualified Distribution.Package as Pkg
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Parse as PD
import qualified Distribution.Text as DT
import qualified Distribution.Version as Vers
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as C
import qualified Network.HTTP.Types.Status as Http
import qualified System.FilePath.Glob as G

getProjectInfo :: IO (Either String ProjectInfo)
getProjectInfo =
    runExceptT $
    do cabalFiles <- liftIO $ G.glob "*.cabal"
       case cabalFiles of
         [] -> throwError "No cabal file in working directory!"
         (f1:_) ->
             do cabalData <- liftIO $ readFile f1
                case PD.parsePackageDescription cabalData of
                  PD.ParseFailed err -> throwError (show err)
                  PD.ParseOk _ val -> compileProjectInfo val

onStackageCheck :: T.Text -> IO Bool
onStackageCheck projectName =
    do mgr <- C.newManager C.tlsManagerSettings
       initReq <- C.parseUrl ("https://www.stackage.org/package/" ++ T.unpack projectName)
       let tryGet =
               do resp <- C.httpNoBody initReq mgr
                  return $ C.responseStatus resp == Http.ok200
       tryGet `catch` \(_ :: SomeException) -> return False

onHackageCheck :: T.Text -> IO Bool
onHackageCheck projectName =
    do mgr <- C.newManager C.tlsManagerSettings
       initReq <- C.parseUrl ("https://hackage.haskell.org/package/" ++ T.unpack projectName)
       let tryGet =
               do resp <- C.httpNoBody initReq mgr
                  return $ C.responseStatus resp == Http.ok200
       tryGet `catch` \(_ :: SomeException) -> return False

findGhcVersions :: [(Comp.CompilerFlavor, Vers.VersionRange)] -> ExceptT String IO [T.Text]
findGhcVersions origVersions =
    forM versions $ \vers ->
        let loop [] = throwError ("Unknown ghc version: " ++ show vers)
            loop (x:xs) =
                if Vers.withinRange x vers then return x else loop xs
        in liftM (T.pack . DT.display) $ loop ghcLatest
    where
      versions = map snd $ filter (\(flavor, _) -> flavor == Comp.GHC) origVersions
      ghcLatest =
          [ Vers.Version [7, 4, 2] []
          , Vers.Version [7, 6, 3] []
          , Vers.Version [7, 8, 4] []
          , Vers.Version [7, 10, 2] []
          ]

getCliUsage :: Bool -> [String] -> ExceptT String IO [CliExecutable]
getCliUsage hasStack exec =
    forM exec $ \program ->
    do (ec, stdOut, stdErr) <-
           liftIO $
           if hasStack
           then readProcessWithExitCode "/bin/bash" ["-c", "stack exec -- " ++ program ++ " --help"] ""
           else readProcessWithExitCode "/bin/bash" ["-c", "cabal run -- " ++ program ++ " --help"] ""
       when (ec /= ExitSuccess) $
            throwError $ "Failed to run " ++ program ++ " --help to retrieve cli usage. StdOut was: " ++ stdOut ++ " \n StdErr was: " ++ stdErr
       return
          CliExecutable
          { ce_name = T.pack program
          , ce_help = T.pack stdOut
          }

moreFile :: ExceptT String IO (Maybe T.Text)
moreFile =
    liftIO $
    do more <- doesFileExist "MORE.md"
       if more
       then do ct <- readFile "MORE.md"
               return (Just $ T.pack ct)
       else return Nothing

compileProjectInfo :: PD.GenericPackageDescription -> ExceptT String IO ProjectInfo
compileProjectInfo gpd =
    do let pkgName = T.pack $ Pkg.unPackageName $ Pkg.pkgName $ PD.package pd
       ghInfo <-
           case filter repoFilter $ PD.sourceRepos pd of
             [] -> throwError "No head github source-repository given in cabal file!"
             (repo:_) ->
                 case PD.repoLocation repo of
                   Nothing ->
                       throwError "Missing source-repository location"
                   Just loc -> extractGithub loc
       hasStack <- liftM (not . L.null) $ liftIO $ G.glob "stack*.yaml"
       (example, hasMoreEx) <-
           do files <- liftIO $ G.glob "examples/*.hs"
              forM_ files $ \file ->
                  do res <-
                         liftIO $
                         if hasStack
                         then system $ "stack exec -- ghc -fno-code " ++ file
                         else system $ "cabal exec -- ghc -fno-code " ++ file
                     when (res /= ExitSuccess) $ throwError $ "Failed to compile " ++ file
              case files of
                [] -> return (Nothing, False)
                (file:xs) ->
                    do ex <- liftIO $ readFile file
                       return (Just $ T.pack ex, not $ L.null xs)
       onStackage <- liftIO $ onStackageCheck pkgName
       onHackage <- liftIO $ onHackageCheck pkgName
       ghcVers <- findGhcVersions (PD.testedWith pd)
       cliUsage <- getCliUsage hasStack (map fst $ PD.condExecutables gpd)
       moreFile <- moreFile
       return
           ProjectInfo
           { pi_name = pkgName
           , pi_pkgName = pkgName
           , pi_pkgDesc = T.pack $ PD.synopsis pd
           , pi_stackFile = hasStack
           , pi_onStackage = onStackage
           , pi_onHackage = onHackage
           , pi_example = example
           , pi_moreExamples = hasMoreEx
           , pi_github = ghInfo
           , pi_license =
               LicenseInfo
               { li_copyright = T.pack $ PD.copyright pd
               , li_type = T.pack $ DT.display $ PD.license pd
               }
           , pi_ghcVersions = ghcVers
           , pi_cliUsage = cliUsage
           , pi_moreInfo = moreFile
           }
    where
      pd = PD.packageDescription gpd
      repoFilter rep =
          PD.repoKind rep == PD.RepoHead
          && PD.repoType rep == Just PD.Git
          && checkGithub (PD.repoLocation rep)
      checkGithub r =
          case r of
            Nothing -> False
            Just str -> "github.com" `L.isInfixOf` str
      extractGithub loc =
          case L.stripPrefix "https://github.com/" loc of
            Nothing ->
                throwError "source-repository location must start with https://github.com/"
            Just rest ->
                let (usr, slashedRepo) = L.break (=='/') rest
                in return
                   GithubInfo
                   { gi_user = T.pack usr
                   , gi_project = T.pack $ L.drop 1 slashedRepo
                   }
