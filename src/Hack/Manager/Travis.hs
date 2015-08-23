{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Hack.Manager.Travis
    ( renderTravis
    , TravisOpts (..)
    )
where

import Hack.Manager.Types

import Control.Monad
import Data.Data
import Data.FileEmbed
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Text.Hastache as H
import qualified Text.Hastache.Context as H

data TravisOpts
   = TravisOpts
   { to_ghcRelease :: T.Text
   , to_useStack :: Bool
   } deriving (Show, Eq, Data, Typeable)

data TravisCtx
   = TravisCtx
   { tc_project :: ProjectInfo
   , tc_opts :: TravisOpts
   } deriving (Show, Eq, Data, Typeable)

stackTemplate :: BS.ByteString
stackTemplate = $(embedFile "templates/travis-stack.mustache")

cabalTemplate :: BS.ByteString
cabalTemplate = $(embedFile "templates/travis-cabal.mustache")

renderTravis :: TravisOpts -> ProjectInfo -> IO T.Text
renderTravis to pinfo =
   liftM TL.toStrict $ H.hastacheStr cfg (T.decodeUtf8 tpl) ctx
    where
      tpl =
          if to_useStack to then stackTemplate else cabalTemplate
      ctx =
          H.mkGenericContext $
          TravisCtx
          { tc_project = pinfo
          , tc_opts = to
          }
      cfg =
          H.defaultConfig
          { H.muTemplateFileDir = Nothing
          , H.muEscapeFunc = H.emptyEscape
          }
