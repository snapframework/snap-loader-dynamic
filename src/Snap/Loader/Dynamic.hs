{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module includes the machinery necessary to use hint to load
-- action code dynamically.  It includes a Template Haskell function
-- to gather the necessary compile-time information about code
-- location, compiler arguments, etc, and bind that information into
-- the calls to the dynamic loader.
module Snap.Loader.Dynamic
  ( loadSnapTH
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad (liftM2, forever)
#if !MIN_VERSION_base(4,7,0)
import           Data.Char (isAlphaNum)
#endif
import           Data.List
import           Data.Maybe (maybeToList)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Typeable
import           Language.Haskell.Interpreter hiding (lift, typeOf)
import           Language.Haskell.Interpreter.Unsafe
import           Language.Haskell.TH
import           System.Environment (getArgs)
import           Snap.Core
import           Snap.Loader.Dynamic.Signal
import           Snap.Loader.Dynamic.Evaluator
import           Snap.Loader.Dynamic.TreeWatcher


------------------------------------------------------------------------------
-- | This function derives all the information necessary to use the interpreter
-- from the compile-time environment, and compiles it in to the generated code.
--
-- This could be considered a TH wrapper around a function
--
-- > loadSnap :: Typeable a => IO a -> (a -> IO (Snap (), IO ()))
-- >                        -> [String] -> IO (a, Snap (), IO ())
--
-- with a magical implementation. The [String] argument is a list of
-- directories to watch for updates to trigger a reloading. Directories
-- containing code should be automatically picked up by this splice.
--
-- The generated splice executes the initialiser once, sets up the interpreter
-- for the load function, and returns the initializer's result along with the
-- interpreter's proxy handler and cleanup actions. The behavior of the proxy
-- actions will change to reflect changes in the watched files, reinterpreting
-- the load function as needed and applying it to the initializer result.
--
-- This will handle reloading the application successfully in most cases. The
-- cases in which it is certain to fail are those involving changing the types
-- of the initializer or the load function, or changing the compiler options
-- required, such as by changing/adding dependencies in the project's .cabal
-- file. In those cases, a full recompile will be needed.
--
loadSnapTH :: Q Exp      -- ^ the initializer expression
           -> Name       -- ^ the name of the load function
           -> [String]   -- ^ a list of directories to watch in addition
                         --   to those containing code
           -> Q Exp
loadSnapTH initializer action additionalWatchDirs = do
    args <- runIO getArgs

    let opts     = getHintOpts args
        srcPaths = additionalWatchDirs ++ getSrcPaths args

    -- The first line is an extra type check to ensure the arguments
    -- provided have the the correct types
    [| do let _ = $initializer >>= $(varE action)
          v <- $initializer
          (handler, cleanup) <- hintSnap opts actMods srcPaths loadStr v
          return (v, handler, cleanup) |]
  where
    actMods = maybeToList $ nameModule action
    loadStr = nameBase action


------------------------------------------------------------------------------
-- | Convert the command-line arguments passed in to options for the
-- hint interpreter.  This is somewhat brittle code, based on a few
-- experimental datapoints regarding the structure of the command-line
-- arguments cabal produces.
getHintOpts :: [String] -> [String]
getHintOpts args = removeBad opts
  where
    --------------------------------------------------------------------------
    bad       = ["-threaded", "-O", "-main-is", "-o", "--make", "-static"]

    --------------------------------------------------------------------------
    removeBad = filter (\x -> not $ any (`isPrefixOf` x) bad)

    --------------------------------------------------------------------------
    hideAll   = filter (== "-hide-all-packages") args

    --------------------------------------------------------------------------
    srcOpts   = filter (\x -> "-i" `isPrefixOf` x) args

    --------------------------------------------------------------------------
    toCopy    = filter (not . isSuffixOf ".hs") $
                dropWhile (not . ("-package" `isPrefixOf`)) args

    --------------------------------------------------------------------------
    copy      = map (intercalate " ")
                    . groupBy (\_ s -> not $ "-" `isPrefixOf` s)

    --------------------------------------------------------------------------
    opts      = concat [hideAll, srcOpts, copy toCopy]


------------------------------------------------------------------------------
-- | This function extracts the source paths from the compilation args
getSrcPaths :: [String] -> [String]
getSrcPaths = filter (not . null) . map (drop 2) . filter srcArg
  where
    srcArg x = "-i" `isPrefixOf` x && not ("-idist" `isPrefixOf` x)


------------------------------------------------------------------------------
-- | This function creates the Snap handler that actually is responsible for
-- doing the dynamic loading of actions via hint, given all of the
-- configuration information that the interpreter needs. It also ensures safe
-- concurrent access to the interpreter, and caches the interpreter results for
-- a short time before allowing it to run again.
--
-- Generally, this won't be called manually. Instead, loadSnapTH will generate
-- a call to it at compile-time, calculating all the arguments from its
-- environment.
--
hintSnap :: Typeable a
         => [String]
             -- ^ A list of command-line options for the interpreter
         -> [String]
             -- ^ A list of modules that need to be interpreted. This should
             -- contain only the modules which contain the initialization,
             -- cleanup, and handler actions. Everything else they require will
             -- be loaded transitively.
         -> [String]
             -- ^ A list of paths to watch for updates
         -> String
             -- ^ The name of the function to load
         -> a
             -- ^ The value to apply the loaded function to
         -> IO (Snap (), IO ())
hintSnap opts modules srcPaths action value = do
    load <- runInterpreterThread
    protectedHintEvaluator getCurrentState testState load
  where
    --------------------------------------------------------------------------
    witness x = undefined $ x `asTypeOf` value :: HintLoadable

#if MIN_VERSION_base(4,7,0)
    --------------------------------------------------------------------------
    witnessModules = filter (`notElem` inPrelude) . map dropInternal .
                     map tyConModule . tyCons . typeOf $ witness

    --------------------------------------------------------------------------
    inPrelude = ["GHC.Prim", "GHC.Types", "GHC.Tuple"]

    --------------------------------------------------------------------------
    tyCons x = let (c, rs) = splitTyConApp x in c : concatMap tyCons rs

    --------------------------------------------------------------------------
    dropInternal s = case stripPrefix "Snap.Internal." s of
        Nothing      -> s
        Just "Types" -> "Snap.Core"
        Just x       -> "Snap." ++ x
#else
    --------------------------------------------------------------------------
    -- This is somewhat fragile, and probably can be cleaned up with a future
    -- version of Typeable. For the moment, and backwards-compatibility, this
    -- is the approach being taken.
    witnessModules = map (reverse . drop 1 . dropWhile (/= '.') . reverse) .
                     filter (elem '.') . groupBy typePart . show . typeOf $
                     witness

    --------------------------------------------------------------------------
    typePart x y = (isAlphaNum x && isAlphaNum  y) || x == '.' || y == '.'
#endif

    --------------------------------------------------------------------------
    runInterpreterThread = do
        input  <- newEmptyMVar
        output <- newEmptyMVar    
        
        forkIO . forever $ do
            restore <- protectHandlers
            err <- unsafeRunInterpreterWithArgs opts $ do
                liftIO $ restore
                forever $ do
                    liftIO $ takeMVar input

                    loadModules . nub $ modules
                    setImports . nub $ "Prelude" : "Snap.Core" :
                        witnessModules ++ modules
                    f <- interpret action witness

                    liftIO . putMVar output $ f value
                    reset

            putMVar output $ formatOnError err

        return $ putMVar input () >> takeMVar output

    --------------------------------------------------------------------------
    formatOnError (Left err) = error $ format err
    formatOnError (Right a) = a

    --------------------------------------------------------------------------
    getCurrentState = liftM2 (,) getCurrentTime $ getTreeStatus srcPaths

    --------------------------------------------------------------------------
    testState (prevTime, ts) = do
        now <- getCurrentTime
        if diffUTCTime now prevTime < 3
            then return True
            else checkTreeStatus ts


------------------------------------------------------------------------------
-- | Convert an InterpreterError to a String for presentation
format :: InterpreterError -> String
format (UnknownError e)   = "Unknown interpreter error:\r\n\r\n" ++ e
format (NotAllowed e)     = "Interpreter action not allowed:\r\n\r\n" ++ e
format (GhcException e)   = "GHC error:\r\n\r\n" ++ e
format (WontCompile errs) = "Compile errors:\r\n\r\n" ++
    (intercalate "\r\n" $ nub $ map errMsg errs)
