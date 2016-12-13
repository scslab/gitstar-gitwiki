{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Welcome (welcomeView) where

import Prelude hiding (div, span)
import Data.Maybe (fromMaybe)
import Control.Monad (void)

import Gitstar.Models(UserName, ProjectName)

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (id, label, form, span)

welcomeView :: Maybe UserName -> ProjectName -> Html
welcomeView musr proj =
  div ! class_ "hero-unit" $ do
    h1 "Gitstar Git Wiki"
    p "A simple git-based wki app. To create the wiki execute: "
    div $ pre ! class_ "prettyprint" $ do
       toHtml $ "$ cd " ++ usr ++"/" ++ proj ++ "\n"
       void "$ git checkout -b wiki\n"
       void "# remove all but .git\n"
       void "$ echo \"# My Wiki\" > Home.md\n"
       void "$ git add Home.md\n"
       void "$ git commit -m \"my first wiki entry\"\n"
       void "$ git push origin wiki:wiki\n"
    where usr = fromMaybe "username" musr
