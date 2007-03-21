{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

module MacroList where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import qualified Graphics.UI.Gtk.ModelView as MV
import System.Directory
import Metacity
import Utils
import Buttons

{- | Initialize the storage directory -}
initDir xml = do
    dir <- getAppUserDataDirectory "gmacro"
    createDirectoryIfMissing False dir
    return dir

{- | Initialize the display list -}
initList xml buttons = 
    do list <- xmlGetWidget xml MV.castToTreeView "rectree"
       model <- MV.listStoreNew [("fake", "fake")]
       MV.treeViewSetModel list model
       render <- MV.cellRendererTextNew
       render2 <- MV.cellRendererTextNew

       namecol <- MV.treeViewColumnNew
       bindcol <- MV.treeViewColumnNew

       MV.treeViewColumnSetTitle namecol "Macro"
       MV.treeViewColumnSetTitle bindcol "Connected Shortcut"
       MV.treeViewColumnPackStart namecol render True
       MV.treeViewColumnPackStart bindcol render2 True
       MV.cellLayoutSetAttributes namecol render model $ \row -> 
             [MV.cellText := fst row]
       MV.cellLayoutSetAttributes bindcol render2 model $ \row ->
             [MV.cellText := snd row]

       MV.treeViewColumnSetSizing bindcol TreeViewColumnAutosize
                  
       MV.treeViewAppendColumn list namecol
       MV.treeViewAppendColumn list bindcol

       MV.treeViewSetHeadersVisible list True

       selection <- treeViewGetSelection list
       treeSelectionSetSelectFunction selection selectfunc
       return (list, model)
    where selectfunc [] = disablePerMacro buttons >> return True
          selectfunc _ = enablePerMacro buttons >> return True


{- | Load the files into the list -}
loadList model macdir = do
    dir <- getAppUserDataDirectory "gmacro"
    files' <- getDirectoryContents dir
    let files = filter (\f -> f /= "." && f /= "..") files'
    bindings <- getMacroBindings

    MV.listStoreClear model
    mapM_ (addrow bindings) files
    where addrow bindings file = 
           MV.listStoreAppend model (file, binding)
           where binding = case lookup file bindings of
                              Nothing -> "none"
                              Just x -> x

