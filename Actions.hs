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
    (_, macro, _, ph) <- runInteractiveCommand 
       "xmacrorec2 -k 0xffff | egrep -v '^(ButtonRelease|ButtonPress|MotionNotify)' 2>/dev/null" 
    -- Copy the data in a separate thread so we don't interfere with GUI
    mv <- newEmptyMVar
    forkIO $ do hSetBuffering macro NoBuffering
                macroc <- hGetContents macro
                hPutStr recordh macroc
                putMVar mv ()
    
    -- Intercept the click of the close button.  Don't let it destroy
    -- the widget (we'll need it again for the next recording)
    onDelete recordwin (\_ -> recorddone recordwin macro ph recordh mv >> 
                              return True)
    onClicked finishedbt (recorddone recordwin macro ph recordh mv)
    windowPresent recordwin
    return ()
    where recorddone recordwin macro ph recordh mv = do
              terminateProcess ph
              takeMVar mv
              widgetHide recordwin
              hClose macro
              hClose recordh
              loadList model macdir

