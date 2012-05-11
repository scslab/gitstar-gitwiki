{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe (fromJust)

import Data.IterIO.Http.Support
import Hails.Data.LBson (genObjectId)

import Control.Monad
import Control.Monad.Trans.State

import LIO
import LIO.DCLabel

import Data.IterIO.Http (respAddHeader)


-- | Force get parameter value
getParamVal :: Monad m => S8.ByteString -> Action t b m String
getParamVal n = (L8.unpack . paramValue . fromJust) `liftM` param n

with404orJust :: Monad m => Maybe a -> (a -> Action t b m ()) -> Action t b m ()
with404orJust mval act = case mval of
                           Nothing -> respond404
                           Just val -> act val

--
-- Flash notifications
--

-- | This sets the @_flash-*@ cookie value to the given message, with
-- a unique message ID.
flash :: String -> String -> Action t b DC ()
flash n msg = do
  oid <- liftLIO genObjectId
  modify $ \s ->
    let flashHeader = (S8.pack "Set-Cookie",
          S8.pack $ "_flash-" ++ n ++ "=" ++ show (show oid ++ "|" ++ msg))
    in s { actionResp = respAddHeader flashHeader (actionResp s)}

flashInfo :: String -> Action t b DC ()
flashInfo = flash "info"

flashError :: String -> Action t b DC ()
flashError = flash "error"

flashSuccess :: String -> Action t b DC ()
flashSuccess = flash "success"

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs@(_:_) = tail xs
