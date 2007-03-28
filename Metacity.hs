{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

module Metacity where

import System.Gnome.GConf.GConfClient
import Utils
import Data.List
import Data.Maybe(catMaybes)

{- | Gets the currently bound macros.  Returns [(Name, Shortcut)]. -}
getMacroBindings :: IO [(String, String)]
getMacroBindings = do
    gc <- gconfGetDefault
    bindings <- getBindings gc >>= return . filter ((/=) "disabled" . snd)
    commands <- getCommands gc >>= return . filter ((/=) "" . snd)
    putStrLn $ "GMB: b: " ++ show bindings
    putStrLn $ "GMB: c: " ++ show commands

    return . (flip combine) bindings . 
       map (\(x, y) -> (x, drop (length "gmacroplay ") y)) .
       filter (isPrefixOf "gmacroplay " . snd) $
       commands

-- Next 3 funcs: Convert both commands and bindings to ("command_x", value)
gc2str [] = []
gc2str ((x, GConfValueString s):xs) = (x, s):gc2str xs
gc2str (_:xs) = gc2str xs

getBindings gc =
    do bindings <- gconfAllEntries gc dir
       return $ map (\(name, val) -> (drop (length dir + 5) name, val)) .
          filter (isPrefixOf (dir ++ "/run_command_") . fst) . 
          gc2str $ bindings
    where dir = "/apps/metacity/global_keybindings"

getCommands gc =
    do commands <- gconfAllEntries gc dir
       return $ map (\(x, y) -> (drop (length dir + 1) x, y)) .
                   filter (isPrefixOf (dir ++ "/command_") . fst) . 
                   gc2str $ commands
    where dir = "/apps/metacity/keybinding_commands"


{- | Binds a new macro. -}
bindMacro :: String                         -- ^ macro name
          -> String                         -- ^ keyboard shortcut
          -> IO ()
bindMacro name shortcut = 
    do gc <- gconfGetDefault
       bindings <- getBindings gc
       commands <- getCommands gc
       let toremove_cmds = map fst . filter ((==) shortcut . snd) $ bindings
       let toremove_names = map (drop (length "gmacroplay ")) .
                               catMaybes . map (\x -> lookup x commands)
                               $ toremove_cmds
       let command = findCommand bindings commands
       gconfSet gc ("/apps/metacity/global_keybindings/run_" ++ command)
           shortcut
       gconfSet gc ("/apps/metacity/keybinding_commands/" ++ command)
           ("gmacroplay " ++ name)
       mapM_ removeBinding toremove_names
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
       putStrLn $ "removeBinding " ++ name
       bindings <- getBindings gc
       print 78
       commands <- getCommands gc
       print 80
       let cmd = find ((==) ("gmacroplay " ++ name) . snd) commands
       case cmd of
            Nothing -> return () -- No command to unbind
            Just (x,_) -> do 
               print 85
               gconfSet gc ("/apps/metacity/global_keybindings/run_"
                            ++ x) "disabled"
               print 88
               gconfSet gc ("/apps/metacity/keybinding_commands/" ++ x) ""
