{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Wiki ( showWikiHome
                        , showWikiPage
                        , showPages
                        ) where

import Layouts
import Views.Welcome
import Views.Wiki
import Utils

import Control.Monad

import LIO
import LIO.DCLabel

import Gitstar.Repo

import qualified Data.List as List
import Data.Maybe
import Data.IterIO.Http
import Data.IterIO.Http.Support

import qualified Data.ByteString.Char8 as S8

import System.FilePath (splitDirectories)


homeFileName :: String
homeFileName = "Home.md"


-- | Show wiki home page
showWikiHome :: Action t b DC ()
showWikiHome = do
  uName <- getParamVal "user_name"
  pName <- getParamVal "project_name"
  let repo = Repo { repoOwner = uName, repoName = pName }
  doShowPage repo [homeFileName]


-- | Show a page
showWikiPage :: Action t b DC ()
showWikiPage = do
  uName <- getParamVal "user_name"
  pName <- getParamVal "project_name"
  req   <- getHttpReq
  let path = S8.unpack . reqPath $ req
      repo = Repo { repoOwner = uName, repoName = pName }
      dirs = drop 3 $ splitDirectories' path -- rm /usr/proj/page/
  doShowPage repo dirs

-- | Show all files in the branch
showPages :: Action t b DC ()
showPages = do
  uName <- getParamVal "user_name"
  pName <- getParamVal "project_name"
  let repo = Repo { repoOwner = uName, repoName = pName }
  mbranches <- liftLIO $ getBranches repo
  withHelpOrJust repo mbranches $ \bs -> 
    withHelpOrJust repo (List.lookup "wiki" bs) $ \sha -> do
      mtree <- liftLIO $ getTree repo sha
      withHelpOrJust repo mtree $ \tree -> do
        pages <- liftLIO $ getAllBlobs repo "" tree
        renderHtml $ viewPages repo pages


-- | Actually show a page given a path to file
doShowPage :: Repo -> [String] -> Action t b DC ()
doShowPage repo dirs = do
  mbranches <- liftLIO $ getBranches repo
  withHelpOrJust repo mbranches $ \bs -> 
    withHelpOrJust repo (List.lookup "wiki" bs) $ \sha -> 
      if null dirs
        then respondStat stat500
        else do let objName = last dirs
                mtree <- liftLIO $ getTree repo sha
                withHelpOrJust repo mtree $ \tree -> do
                  mt <- liftLIO $ getTreeByPath repo tree (init dirs)
                  withHelpOrJust repo mt $ \t -> 
                    let mEnt = listToMaybe $ filter ((==objName) . entPath) t
                    in withHelpOrJust repo mEnt $ \ent -> do
                         mblob <- liftLIO $ getBlob repo (entPtr ent)
                         with404orJust mblob $ \blob -> renderHtml $
                           viewWiki repo tabType blob
  where tabType = if dirs == [homeFileName]
                    then ViewHome
                    else ViewOther $ List.intercalate "/" dirs
--
-- Helpers
--

-- | Given a repo, path prefix and tree recursive get all the blobs.
getAllBlobs :: Repo -> String -> GitTree -> DC [String]
getAllBlobs repo prefix = foldM (\acc ent -> 
  case entType ent of
    GtBlob -> return $ acc ++ [prefix ++ "/" ++ entPath ent]
    GtTree -> do mt <- getTree repo (entPtr ent)
                 maybe (return acc)
                       (getAllBlobs repo (prefix ++ "/" ++ entPath ent))
                       mt
    _ -> return acc
  ) [] 

-- | Find the tree given a head tree object and remaining tree names
getTreeByPath :: Repo -> GitTree -> [String] -> DC (Maybe GitTree)
getTreeByPath repo headTree dirs = 
  if null dirs
    then return $ Just headTree
    else do let n = head dirs
                newPath = safeTail dirs
                mEnt = listToMaybe $ filter ((==n) . entPath) headTree
            case mEnt of
              Nothing -> return Nothing
              Just ent -> do mtree <- getTree repo (entPtr ent)
                             case mtree of
                               Nothing -> return Nothing
                               Just tree -> getTreeByPath repo tree newPath


-- | Same as 'splitdirectories', but does not keep the first slash.
splitDirectories' :: FilePath -> [FilePath]
splitDirectories' p = let ds = splitDirectories p
                      in case ds of
                          ("/":xs) -> xs
                          xs -> xs


-- | Same as 'with404orJust', but prints out create wiki message.
withHelpOrJust :: Repo
               -> Maybe a
               -> (a -> Action t b DC ())
               -> Action t b DC ()
withHelpOrJust repo mx f = do
  maybe (renderHtml $ welcomeView uName pName) f mx
    where uName = repoOwner repo
          pName = repoName repo
