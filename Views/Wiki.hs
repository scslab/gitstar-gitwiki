{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Wiki ( viewWiki
                  , viewPages
                  , ViewTab(..)
                  ) where

import Prelude hiding (div, span, id)

import Control.Monad

import qualified Data.ByteString.Char8 as S8

import Text.Blaze.Html5 hiding (title, style, map)
import Text.Blaze.Html5.Attributes hiding (label, form, span, title)

import Text.Pandoc

import Gitstar.Repo


data ViewTab = ViewHome
             | ViewPages
             | ViewOther String
  deriving (Eq, Show)


-- | Show a page
viewPage :: GitBlob -> Html
viewPage = markdownToHtml . S8.unpack . blobContent

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
  div $ viewPage blob
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

markdownToHtml :: String -> Html
markdownToHtml str =
  writeHtml defaultWriterOptions (readMarkdown defaultParserState str)

repo2url :: Repo -> String
repo2url r = "/" ++ repoOwner r ++ "/" ++ repoName r
