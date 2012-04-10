{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE OverloadedStrings #-}

module Layouts where

import LIO
import Policy.Gitstar
import Data.IterIO.Http.Support

import Hails.App

import Prelude hiding (head, id, div, span)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (title, span, content)
import qualified Text.Blaze.Renderer.Utf8 as R (renderHtml)

renderHtml :: Html -> Action t b DC ()
renderHtml htmlBody = do
  uName <- getHailsUser
  user <- liftLIO $ getOrCreateUser uName
  render "text/html" $ R.renderHtml $ application user htmlBody

stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" ! type_ "text/css" ! href (toValue uri)

application :: User -> Html -> Html
application _ content = docTypeHtml $ do
  head $ do
    title $ "GitStar - Where loops count"
    stylesheet "/static/css/bootstrap.css"
    stylesheet "/static/css/application.css"
    stylesheet "/static/css/prettify.css"
    body ! onload "prettyPrint()" $ do
     div ! class_ "row" $
       div ! id "flash-messages" ! class_ "span4 offset4" $ ""
     div ! class_ "container" $ content
     script ! src "/static/js/jquery.js" $ ""
     script ! src "/static/js/jquery.cookie.js" $ ""
     script ! src "/static/js/bootstrap.min.js" $ ""
     script ! src "/static/js/prettify.js" $ ""
     script ! src "/static/js/flash.js" $ ""

