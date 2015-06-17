-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.WindowNames
-- Copyright   :  (c) 2015 Phil Lindberg <plindbe2@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Phil Lindberg <plindbe2@gmail.com>
-- Stability   :  experimental
-- Portability :  unportable
--
-- A library to automatically put named windows into the DynamicLog.  It
-- provides a modification of 'XMonad.Hooks.DynamicLog.dynamicLogWithPP' and
-- some helper functions.
--
-----------------------------------------------------------------------------

module XMonad.Actions.WindowNames (
        -- * Usage
        -- $usage

        -- * Modified dynamic log functions
          dynamicLogStringMap
        , dynamicLogWithPPMap
        -- * Helper functions
        , evalLookup
        , filterShortNames
        -- * Stack and StackSet functions
        , getTagStack
        , getWindowNames
        , integrateM
        -- * Pretty-printer functions
        , pprWindowSetMap
        -- * To Do
        -- $todo
    ) where

import Codec.Binary.UTF8.String (encodeString)
import Control.Monad (mapM, MonadPlus(mplus))
import Data.List (intercalate)
import Data.Maybe (listToMaybe, catMaybes, fromMaybe, isJust)
import XMonad
import XMonad.Hooks.DynamicLog (PP(..))
import XMonad.Hooks.UrgencyHook (readUrgents)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.WorkspaceCompare (WorkspaceSort)

import qualified Data.Map as M
import qualified XMonad.StackSet as W


-- $usage
-- You can add this to your @~\/.xmonad\/xmonad.hs@:
--
-- >    import Data.List (isSuffixOf)
-- >    import System.IO
-- >    import WindowNames
-- >    import XMonad
-- >    import XMonad.Hooks.DynamicLog
-- >    import XMonad.Util.Run(spawnPipe)
--
-- A simple example is a follows (the example uses xmobar but feel free to
-- substitute dzen or anything else):
--
-- > myShortNames :: [(String -> Bool, String -> String)]
-- > myShortNames = [ (("Pentadactyl" `isSuffixOf`), const "ff"  )
-- >                , ((=="Library"),                const "ffdl")
-- >                , (const True,                   take 4      ) -- catch all
-- >                ]
-- >
-- > main = do
-- >     h <- spawnPipe "xmobar -options -foo -bar"
-- >     xmonad $ myXmonadConfig h
-- >
-- > myXmonadConfig h = defaultConfig {
-- >   ...
-- >   logHook = dynamicLogWithPPMap (getWindowNames (foc . filt) filt filt) xmobarPP { ppOutput = hPutStrLn h }
-- >   ...
-- > }
-- >  where foc  = wrap "<fn=1>" "</fn>" -- xmobar font 1 (additionalFonts!!0)
-- >        filt = filterShortNames myShortNames
--
-- This works nicely with named tmux sessions or screens.  You can use it in
-- conjunction with other dynamicLog string modifiers.

-- $todo
-- * Incorporate with 'XMonad.Actions.TagWindows'
-- * Efficiency (ExtensionClass?)
-- * Make sure it works with the newest XMonad

-- | Helper function used by 'filterShortNames' which run the first function in
--   the tuple on the value.  If it evalutes to true, the output of the second
--   function if returned.
evalLookup :: MonadPlus m => a -> [(a -> Bool, a -> b)] -> m b
evalLookup n ((f,g):xs) | f n       = return (g n) `mplus` evalLookup n xs
                        | otherwise = evalLookup n xs

-- | Example filter short name function.
filterShortNames :: [(String -> Bool, String -> String)] -> String -> String
filterShortNames filterList s = fromMaybe "" $ evalLookup s filterList

-- | Get a Map of window tag to strings in the format "wsId:w1,w2,...,wN" where
--   w1,...,wn represent the names of windows according to
--   'XMonad.Util.NamedWindows.getName' after being run through the respective
--   focus, up, and down functions
getWindowNames :: (String -> String)                     -- ^ Function to apply to the focused window's name.
               -> (String -> String)                     -- ^ Function to apply to the up windows' names.
               -> (String -> String)                     -- ^ Function to apply to the down windows' names.
               -> W.StackSet WorkspaceId l Window sid sd -- ^ Stack set
               -> X (M.Map WorkspaceId String)
getWindowNames f u d w = fmap M.fromList $ mapM returnNameStr $ getTagStack w
  where joinWinNames x _ = fmap (((x ++ ":") ++) . intercalate "," . filter (not . null)) .
                           integrateM (nameFunc f) (nameFunc u) (nameFunc d)

        returnNameStr (x, y) = maybe (return x) (joinWinNames x y) y >>= \z -> return (x, z)
        nameFunc f = fmap (f . show) . getName

-- | Run a monadic function on 'XMonad.StackSet.Stack' components.
integrateM :: Monad m =>
              (a -> m b) -- ^ Monadic function to run on focused components.
           -> (a -> m b) -- ^ Monadic function to run on up components.
           -> (a -> m b) -- ^ Monadic function to run on down components.
           -> W.Stack a  -- ^ Current StackSet
           -> m [b]
integrateM f u d (W.Stack x l r) = do
    r2 <- mapM u r
    x2 <- f x
    l2 <- mapM d l
    return $ reverse l2 ++ x2 : r2

-- | Get a list of tag and maybe Stack.
getTagStack :: W.StackSet t l a sid sd -> [(t, Maybe (W.Stack a))]
getTagStack w = [ (i, a) | W.Workspace i _ a <- (W.workspace . W.current) w : map W.workspace (W.visible w) ++ W.hidden w ]

-- | Modified 'XMonad.Hooks.DynamicLog.dynamicLogWithPP' which takes an
--   additional WindowSet to Map of WorkspaceId to String.
dynamicLogWithPPMap :: (WindowSet -> X (M.Map WorkspaceId String)) -> PP -> X ()
dynamicLogWithPPMap wsIdMap pp = dynamicLogStringMap wsIdMap pp >>= io . ppOutput pp

-- | Modifed 'XMonad.Hooks.DynamicLog.dynamicLogString' which takes an
--   additional WindowSet to Map of WorkspaceId to String.
dynamicLogStringMap :: (WindowSet -> X (M.Map WorkspaceId String)) -> PP -> X String
dynamicLogStringMap wsIdMap pp = do
    winset <- gets windowset
    urgents <- readUrgents
    sort' <- ppSort pp

    -- layout description
    let ld = description . W.layout . W.workspace . W.current $ winset

    -- workspace list
    ws <- pprWindowSetMap wsIdMap sort' urgents pp winset

    -- window title
    wt <- maybe (return "") (fmap show . getName) . W.peek $ winset

    -- run extra loggers, ignoring any that generate errors.
    extras <- mapM (`catchX` return Nothing) $ ppExtras pp

    return $ encodeString . sepBy (ppSep pp) . ppOrder pp $
                        [ ws
                        , ppLayout pp ld
                        , ppTitle  pp $ ppTitleSanitize pp wt
                        ]
                        ++ catMaybes extras

-- | Format the workspace information
pprWindowSetMap :: (WindowSet -> X (M.Map WorkspaceId String)) -- ^ Map of WorkspaceId to a string to be displayed.
                -> WorkspaceSort                               -- ^ A workspace sorting function
                -> [Window]                                    -- ^ A list of urgent windows
                -> PP                                          -- ^ pretty-printer format
                -> WindowSet                                   -- ^ current WindowSet
                -> X String
pprWindowSetMap wsIdFunc sort' urgents pp s = do
            wsIdMap <- wsIdFunc s
            return $ sepBy (ppWsSep pp) . map (fmt wsIdMap) . sort' $ map W.workspace (W.current s : W.visible s) ++ W.hidden s
   where this     = W.currentTag s
         visibles = map (W.tag . W.workspace) (W.visible s)

         fmt wsIdMap w = printer pp (fromMaybe "" $ M.lookup (W.tag w) wsIdMap)
          where printer | any (\x -> maybe False (== W.tag w) (W.findTag x s)) urgents  = ppUrgent
                        | W.tag w == this                                               = ppCurrent
                        | W.tag w `elem` visibles                                       = ppVisible
                        | isJust (W.stack w)                                            = ppHidden
                        | otherwise                                                     = ppHiddenNoWindows

-- | Output a list of strings, ignoring empty ones and separating the
--   rest with the given separator.
sepBy :: String   -- ^ separator
      -> [String] -- ^ fields to output
      -> String
sepBy sep = intercalate sep . filter (not . null)
