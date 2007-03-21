{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import qualified Graphics.UI.Gtk.ModelView as MV
import System.Directory

main = do
    initGUI
    Just xml <- xmlNew "gmacro.glade"
    putStrLn "12"
    macdir <- initDir xml
    putStrLn "14"
    (list, model) <- initList xml
    putStrLn "16"
    loadList model macdir
    putStrLn "18"

    window <- xmlGetWidget xml castToWindow "gmacrow"
    onDestroy window mainQuit

    closebt <- xmlGetWidget xml castToButton "closebt"
    onClicked closebt (widgetDestroy window)

    putStrLn "26"
    mainGUI

initDir xml = do
    dir <- getAppUserDataDirectory "gmacro"
    createDirectoryIfMissing False dir
    return dir

initList xml = 
    do list <- xmlGetWidget xml castToTreeView "rectree"
       putStrLn "36"
       model <- MV.listStoreNew [("fake", "fake")]
       putStrLn "38"
       treeViewSetModel list model
       putStrLn "40"
       render <- cellRendererTextNew
       putStrLn "42"
       render2 <- cellRendererTextNew

       putStrLn "41a"

       namecol <- MV.treeViewColumnNew
       bindcol <- MV.treeViewColumnNew

       putStrLn "46"

       MV.treeViewColumnSetTitle namecol "Macro"
       MV.treeViewColumnSetTitle bindcol "Connected Shortcut"
       putStrLn "41"
       MV.cellLayoutSetAttributes namecol render model $ \row -> 
             [MV.cellText := fst row]
       MV.cellLayoutSetAttributes bindcol render2 model $ \row ->
             [MV.cellText := snd row]

       putStrLn "47"
       treeViewColumnSetSizing bindcol TreeViewColumnAutosize
                  
       treeViewAppendColumn list namecol
       treeViewAppendColumn list bindcol

       putStrLn "62"
       
       treeViewSetHeadersVisible list True
       return (list, model)

loadList model macdir = do
    dir <- getAppUserDataDirectory "gmacro"
    files <- getDirectoryContents dir
    putStrLn "70"
    MV.listStoreClear model
    putStrLn "72"
    mapM_ addrow files
    where addrow file = do
              putStrLn file
              MV.listStoreAppend model (file, show file)

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
