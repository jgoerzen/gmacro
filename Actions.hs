{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

module Actions where 
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Buttons
import MacroList
import qualified Metacity

initActions b list model macdir window xml = do
    onClicked (closebt b) (widgetDestroy window)
    onClicked (connectbt b) (connect list model macdir)
    onClicked (disconnectbt b) (disconnectMacro list model macdir)
    onClicked (newbt b) (record xml)

connect list model macdir = do
    items <- getSelectedItems list model
    let (name, shortcut) = head items
    Metacity.bindMacro name "<Ctrl><Alt>t"
    loadList model macdir

disconnectMacro list model macdir = do
    items <- getSelectedItems list model
    mapM_ remove items
    loadList model macdir
    where remove (name, _) = Metacity.removeBinding name

record xml = do
    recordwin <- xmlGetWidget xml castToWindow "recording"
    finishedbt <- xmlGetWidget xml castToButton "recdonebt"

    -- Intercept the click of the close button.  Don't let it destroy
    -- the widget (we'll need it again for the next recording)
    onDelete recordwin (\_ -> recorddone recordwin >> return True)
    onClicked finishedbt (recorddone recordwin)
    windowPresent recordwin
    return ()
    where recorddone recordwin = widgetHide recordwin

