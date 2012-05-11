{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Wiki ( viewWiki
                  , viewPages
                  , ViewTab(..), tabMimeType
                  ) where

import Prelude hiding (div, span, id)

import Control.Monad

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Base64 as B64

import Text.Blaze.Html5 hiding (title, style, map)
import Text.Blaze.Html5.Attributes hiding (label, form, span, title)

import Text.Pandoc

import Gitstar.Repo

import System.FilePath.Posix (takeExtension)
import Hails.IterIO.Mime

data ViewTab = ViewHome
             | ViewPages
             | ViewOther String
  deriving (Eq, Show)


-- | Show a page
viewPage :: String -> GitBlob -> Html
viewPage mime blob =
  let b64content = blobContent $ blob
      supType = takeWhile (/= '/') mime
      cont    = S8.unpack . B64.decodeLenient . blobContent $ blob
  in case mime of
      "text/x-markdown" -> markdownToHtml cont
      "text/html"       -> preEscapedString cont
      _ | supType == "text" -> pretty . toHtml $ cont
      _                     -> pretty . toHtml . S8.unpack $ b64content
    where pretty h = pre ! class_ "prettyprint" $ h

viewWiki :: Repo -> ViewTab -> GitBlob -> Html
viewWiki repo tab blob = do
  ul ! class_ "nav nav-tabs" $ do
    li ! activeIf ViewHome  $
         a ! href (toValue $ repo2url repo) $ "Home"
    li ! activeIf ViewPages $
         a ! href (toValue $ repo2url repo ++ "/pages") $ "Pages"
    case tab of
      ViewOther fname -> li ! class_ "active" $ a ! href "#" $ toHtml fname
      _ -> return ()
  div $ viewPage (tabMimeType tab) blob
    where activeIf x = if (tab == x) 
                         then class_ "active"
                         else class_ ""
                       
viewPages :: Repo -> [String] -> Html
viewPages repo pages = do
  ul ! class_ "nav nav-tabs" $ do
    li $ a ! href (toValue $ repo2url repo) $ "Home"
    li ! class_ "active" $ a ! href (toValue $ repo2url repo ++ "/pages") $ "Pages"
  ul ! class_ "nav nav-tabs nav-stacked" $ 
    forM_ pages $ \page ->
      li $ a ! href (toValue $ repo2url repo ++ "/page" ++ page) $ toHtml page

--
-- Misc
--

-- | Get the mime type based of the tab
tabMimeType :: ViewTab -> String
tabMimeType (ViewOther path) =
  let ext = takeExtension path
  in if null ext
      then "application/octet-stream" -- unknown
      else S8.unpack $ systemMimeMap (tail ext)
tabMimeType _ = "text/x-markdown"

markdownToHtml :: String -> Html
markdownToHtml str =
  writeHtml defaultWriterOptions (readMarkdown defaultParserState str)

repo2url :: Repo -> String
repo2url r = "/" ++ repoOwner r ++ "/" ++ repoName r
