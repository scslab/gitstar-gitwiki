{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
module GitWiki where

import Data.Monoid
import Hails.App
import Data.IterIO.Http.Support
import Control.Monad (void)

import Controllers

server :: AppReqHandler
server = runAction $ do
  req <- getHttpReq
  prms0 <- params
  body <- getBody >>= (liftLIO . unlabel)
  prms1 <- parseParams' req body
  void . setParams $ prms1 ++ prms0
  runActionRoute $ mconcat 
    [ routeTop $ routeAction welcome
    , routeMethod "GET" $
        routePattern "/:user_name/:project_name/pages" $
                     routeAction showPages
    , routeMethod "GET" $
        routePattern "/:user_name/:project_name/page" $
                     routeAction showWikiPage
    , routeMethod "GET" $
        routePattern "/:user_name/:project_name" $
                     routeAction showWikiHome
    ]
