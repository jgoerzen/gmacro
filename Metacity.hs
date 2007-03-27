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
    bindings <- getBindings gc
    commands <- getCommands gc

    return . combine bindings . 
       map (\(x, y) -> (x, drop (length "gmacroplay ") y)) .
       filter (isPrefixOf "gmacroplay " . snd) $
       commands

-- Next 3 funcs: Convert both commands and bindings to ("command_x", value)
-- also keep disabled/empty ones out of the list.
gc2str [] = []
gc2str ((x, GConfValueString s):xs) = (x, s):gc2str xs
gc2str (_:xs) = gc2str xs

getBindings gc =
    do bindings <- gconfAllEntries gc "/apps/metacity/global_keybindings"
       putStrLn $ "getBindings: " ++ show (gc2str bindings)
       return $ map (\(name, val) -> (drop 4 name, val)) .
          filter (isPrefixOf "run_command_" . fst) . 
          filter ((/=) "disabled" . snd) .
          gc2str $ bindings

getCommands gc =
    do commands <- gconfAllEntries gc "/apps/metacity/keybinding_commands"
       putStrLn $ "getCommands: " ++ show (gc2str commands)
       return $ filter (isPrefixOf "command_" . fst) . 
                   filter ((/=) "" . snd) .
                   gc2str $ commands

{- | Binds a new macro. -}
bindMacro :: String                         -- ^ macro name
          -> String                         -- ^ keyboard shortcut
          -> IO ()
bindMacro name shortcut = 
    do gc <- gconfGetDefault
       bindings <- getBindings gc
       commands <- getCommands gc
       print bindings
       print commands
       let command = findCommand bindings commands
       gconfSet gc ("/apps/metacity/global_keybindings/run_" ++ command)
           shortcut
       gconfSet gc ("/apps/metacity/keybinding_commands/" ++ command)
           ("gmacroplay " ++ name)
    where findCommand [] _ = error "Couldn't find available binding"
          findCommand _ [] = error "Couldn't find available command"
          findCommand ((bcmd,bshort):xs) commands =
              if bshort == "disabled"
                 then case lookup bcmd commands of
                           Just "" -> bcmd
                           _ -> findCommand xs commands
                 else findCommand xs commands

{- | Remove binding.  Takes macro name. -}
removeBinding :: String -> IO ()
removeBinding name =
    do gc <- gconfGetDefault
       bindings <- getBindings gc
       commands <- getCommands gc
       let cmd = find ((==) ("gmacroplay " ++ name) . snd) commands
       case cmd of
            Nothing -> fail "Couldn't find command to unbind"
            Just (x,_) -> do 
               gconfSet gc ("/apps/metacity/global_keybindings/run_"
                            ++ x) "disabled"
               gconfSet gc ("/apps/metacity/keybinding_commands/" ++ x) ""
