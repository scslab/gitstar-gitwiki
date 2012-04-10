{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif

module Controllers ( module Controllers.Wiki
                   , module Controllers.Welcome
                   ) where

import Controllers.Welcome
import Controllers.Wiki
