{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import System.Directory

main = do
    initGUI
    Just xml <- xmlNew "gmacro.glade"
    macdir <- initDir xml
    list <- initList xml macdir

    window <- xmlGetWidget xml castToWindow "gmacrow"
    onDestroy window mainQuit

    closebt <- xmlGetWidget xml castToButton "closebt"
    onClicked closebt (widgetDestroy window)

    mainGUI

initDir xml = do
    dir <- getAppUserDataDirectory "gmacro"
    createDirectoryIfMissing False dir
    return dir

initList xml dir = 
    do list <- xmlGetWidget xml castToTreeView "rectree"
       model <- listStoreNew [TMstring, TMstring]
       treeViewSetModel list model
       render <- cellRendererTextNew
       namecol <- treeViewColumnNewWithAttributes "Macro" render []
       bindcol <- treeViewColumnNewWithAttributes "Connected Shortcut" render []
                  
       treeViewAppendColumn list namecol
       treeViewAppendColumn list bindcol >>= print
       
       treeViewSetHeadersVisible list True
       return list

test = do 
    Just xml <- xmlNew "gmacro.glade"
    window <- xmlGetWidget xml castToWindow "window1"
    onDestroy window mainQuit
    clbutton <- xmlGetWidget xml castToButton "button2"
    onClicked clbutton $ do
        widgetDestroy window
    prompt <- xmlGetWidget xml castToLabel "label1"
    txtfield <- xmlGetWidget xml castToEntry "entry1"
    okbutton <- xmlGetWidget xml castToButton "button1"
    onClicked okbutton (updateit txtfield prompt) 
    onKeyPress window (handleKey txtfield prompt)
    mainGUI

handleKey txtfield prompt e
    | eventKeyName e == "Return" = do updateit txtfield prompt
                                      return True
    | otherwise = do putStrLn ("Got " ++ show (eventKeyName e))
                     return False

updateit txtfield prompt = do
    name <- get txtfield entryText
    set prompt [ labelText := "Hello " ++ name ]
