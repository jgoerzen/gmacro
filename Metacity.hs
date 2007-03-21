{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

module Metacity where

import System.Gnome.GConf.GConfClient
import Utils
import Data.List

{- | Gets the currently bound macros.  Returns [(Name, Shortcut)]. -}
getMacroBindings :: IO [(String, String)]
getMacroBindings = do
    gc <- gconfGetDefault
    commands' <- gconfAllEntries gc "/apps/metacity/keybinding_commands"
    bindings' <- gconfAllEntries gc "/apps/metacity/global_keybindings"

    -- Convert both commands and bindings to ("command_x", value)
    -- also keep disabled/empty ones out of the list.
    let commands = filter (isPrefixOf "command_" . fst) . 
                   filter ((/=) "" . snd) .
                   gc2str $ commands'
    let bindings = map (\(name, val) -> (drop 4 name, val)) .
          filter (isPrefixOf "run_command_" . fst) . 
          filter ((/=) "disabled" . snd) .
          gc2str $ bindings'
    
    return . combine bindings . 
       map (\(x, y) -> (x, drop (length "gmacroplay ") y)) .
       filter (isPrefixOf "gmacroplay " . snd) $
       commands
    
    where gc2str [] = []
          gc2str ((x, GConfValueString s):xs) = (x, s):gc2str xs
          gc2str (_:xs) = gc2str xs
{-
{- | Binds a new macro. -}
bindMacro :: String                         -- ^ macro name
          -> String                         -- ^ keyboard shortcut
          -> IO ()

{- | Remove binding.  Takes macro name. -}
removeBinding :: String -> IO ()
-}
