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
import Control.Exception(evaluate)
import Data.List

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
    
    hClose c1
    hClose c2
    -- Process the output in a separate thread
    forkIO $ do macroc <- hGetContents macroh
                hPutStr recordh (unlines . filter wanteditems . lines $ macroc)
                hClose recordh
                hClose macroh

    -- Intercept the click of the close button.  Don't let it destroy
    -- the widget (we'll need it again for the next recording)
    onDelete recordwin (\_ -> recorddone recordwin xmacroph >> 
                              return True)
    onClicked finishedbt (recorddone recordwin xmacroph)
    windowPresent recordwin
    return ()
    where wanteditems inp
            | isPrefixOf "ButtonRelease" inp = False
            | isPrefixOf "ButtonPress" inp = False
            | isPrefixOf "MotionNotify" inp = False
            | otherwise = True
          recorddone recordwin xmacroph = do
              terminateProcess xmacroph
              waitForProcess xmacroph

              widgetHide recordwin
              loadList model macdir

