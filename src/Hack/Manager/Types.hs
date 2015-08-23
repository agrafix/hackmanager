{-# LANGUAGE DeriveDataTypeable #-}
module Hack.Manager.Types where

import Data.Data
import Data.Typeable
import qualified Data.Text as T

data GithubInfo
   = GithubInfo
   { gi_user :: T.Text
   , gi_project :: T.Text
   } deriving (Show, Eq, Data, Typeable)

data LicenseInfo
   = LicenseInfo
   { li_type :: T.Text
   , li_copyright :: T.Text
   } deriving (Show, Eq, Data, Typeable)

data CliExecutable
   = CliExecutable
   { ce_name :: T.Text
   , ce_help :: T.Text
   } deriving (Show, Eq, Data, Typeable)

data ProjectInfo
   = ProjectInfo
   { pi_name :: T.Text
   , pi_pkgName :: T.Text
   , pi_pkgDesc :: T.Text
   , pi_stackFile :: Bool
   , pi_onStackage :: Bool
   , pi_example :: Maybe T.Text
   , pi_moreExamples :: Bool
   , pi_cliUsage :: [CliExecutable]
   , pi_github :: GithubInfo
   , pi_license :: LicenseInfo
   , pi_ghcVersions :: [T.Text]
   } deriving (Show, Eq, Data, Typeable)
