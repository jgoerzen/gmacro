{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

module Actions where 
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Buttons
import MacroList
import qualified Metacity
import System.IO
import Control.Concurrent
import Control.Concurrent.MVar
import System.Process

initActions b list model macdir window xml = do
    onClicked (closebt b) (widgetDestroy window)
    onClicked (connectbt b) (connect list model macdir)
    onClicked (disconnectbt b) (disconnectMacro list model macdir)
    onClicked (newbt b) (record model macdir xml)

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

record model macdir xml = do
    recordwin <- xmlGetWidget xml castToWindow "recording"
    finishedbt <- xmlGetWidget xml castToButton "recdonebt"

    (recordfn, recordh) <- openTempFile macdir "new-"
    (c1, macroh, c2, xmacroph) <- runInteractiveProcess "xmacrorec2"
                                   ["-k", "0xffff"] Nothing Nothing
    --hClose c1
    --hClose c2

    -- runProcess closes macroh and recordh automatically
    grepph <- runProcess "egrep" ["-v", "^(ButtonRelease|ButtonPress|MotionNotify)"]
               Nothing Nothing (Just macroh) (Just recordh) Nothing

    -- Intercept the click of the close button.  Don't let it destroy
    -- the widget (we'll need it again for the next recording)
    onDelete recordwin (\_ -> recorddone recordwin xmacroph grepph >> 
                              return True)
    onClicked finishedbt (recorddone recordwin xmacroph grepph)
    windowPresent recordwin
    return ()
    where recorddone recordwin xmacroph grepph = do
              terminateProcess xmacroph
              waitForProcess xmacroph
              waitForProcess grepph

              widgetHide recordwin
              loadList model macdir

