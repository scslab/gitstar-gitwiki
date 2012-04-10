{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Trustworthy #-}
#endif

import Data.IterIO.Server.TCPServer
import Hails.HttpServer
#if PRODUCTION
import safe GitWiki
#else
import GitWiki
#endif

main = runTCPServer $ secureHttpServer 8082 server
