{-# LANGUAGE CPP #-}
module Snap.Loader.Dynamic.Signal (protectHandlers) where

#ifdef mingw32_HOST_OS

                                 -------------
                                 -- windows --
                                 -------------
------------------------------------------------------------------------------
import GHC.ConsoleHandler as C

saveHandlers :: IO C.Handler
saveHandlers = C.installHandler Ignore

restoreHandlers :: C.Handler -> IO ()
restoreHandlers h = C.installHandler h >> return ()
------------------------------------------------------------------------------


#else

                                  -----------
                                  -- posix --
                                  -----------
------------------------------------------------------------------------------
import qualified System.Posix.Signals as S

helper :: S.Handler -> S.Signal -> IO S.Handler
helper handler signal = S.installHandler signal handler Nothing

signals :: [S.Signal]
signals = [ S.sigQUIT
          , S.sigINT
          , S.sigHUP
          , S.sigTERM
          ]

saveHandlers :: IO [S.Handler]
saveHandlers = mapM (helper S.Ignore) signals

restoreHandlers :: [S.Handler] -> IO ()
restoreHandlers h = sequence_ $ zipWith helper h signals
------------------------------------------------------------------------------

#endif

                                  ----------
                                  -- both --
                                  ----------
------------------------------------------------------------------------------
protectHandlers :: IO (IO ())
protectHandlers = do
    h <- saveHandlers
    return $ restoreHandlers h
------------------------------------------------------------------------------
