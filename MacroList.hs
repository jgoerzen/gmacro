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
import Control.Monad

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
       onSelectionChanged selection (selectfunc list model)
       treeSelectionSetMode selection SelectionMultiple
       return (list, model)
    where selectfunc list model = do
              items <- getSelectedItems list model
              print items
              if length items == 0
                 then disablePerMacro buttons
                 else do enablePerMacro buttons
                         -- Can't disable already-disabled macros
                         when (filter ((/=) "none" . snd) items == [])
                               (widgetSetSensitivity (disconnectbt buttons)
                                False)

              -- Can't rename or connect multiple buttons
              when (length items > 1)
                   (mapM_ (\x -> widgetSetSensitivity x False)
                         [renamebt buttons, connectbt buttons])

getSelectedItems list model = do
    selection <- treeViewGetSelection list
    rows <- treeSelectionGetSelectedRows selection
    mapM row2data rows
    where row2data [i] = MV.listStoreGetValue model i

{- | Load the files into the list -}
loadList list model macdir = do
    dir <- getAppUserDataDirectory "gmacro"
    files' <- getDirectoryContents dir
    let files = filter (\f -> f /= "." && f /= "..") files'
    print "ML79"
    bindings <- getMacroBindings
    print "ML80"

    treeViewGetSelection list >>= treeSelectionUnselectAll
    MV.listStoreClear model
    print "ML84"
    mapM_ (addrow bindings) files
    where addrow bindings file = 
           do print "ML87"
              MV.listStoreAppend model (file, binding)
           where binding = case lookup file bindings of
                              Nothing -> "none"
                              Just x -> x

