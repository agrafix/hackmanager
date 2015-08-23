{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Hack.Manager.Gitignore
    ( renderGitignore )
where

import Hack.Manager.Types

import Control.Monad
import Data.FileEmbed
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Text.Hastache as H
import qualified Text.Hastache.Context as H

giTemplate :: BS.ByteString
giTemplate = $(embedFile "templates/gitignore.mustache")

renderGitignore :: ProjectInfo -> IO T.Text
renderGitignore pinfo =
   liftM TL.toStrict $ H.hastacheStr cfg (T.decodeUtf8 giTemplate) ctx
    where
      ctx = H.mkGenericContext pinfo
      cfg =
          H.defaultConfig
          { H.muTemplateFileDir = Nothing
          , H.muEscapeFunc = H.emptyEscape
          }
