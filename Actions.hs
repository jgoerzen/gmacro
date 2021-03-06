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
import System.Directory
import Control.Exception(evaluate)
import Data.List
import Text.Printf
import Control.Monad(when)

initActions b list model macdir window xml = do
    onClicked (closebt b) (widgetDestroy window)
    onClicked (connectbt b) (connect list model macdir)
    onClicked (disconnectbt b) (disconnectMacro list model macdir)
    onClicked (newbt b) (record list model macdir xml)
    onClicked (removebt b) (remove list model macdir window)
    onClicked (renamebt b) (rename list model macdir xml)

connect list model macdir = do
    items <- getSelectedItems list model
    let (name, shortcut) = head items
    Metacity.bindMacro name "<Ctrl><Alt>t"
    loadList list model macdir

disconnectMacro list model macdir = do
    items <- getSelectedItems list model
    mapM_ removebind items
    loadList list model macdir
    where removebind (name, _) = Metacity.removeBinding name

remove list model macdir window = do
    items <- getSelectedItems list model
    let itemstr = concat . intersperse ", " . map fst $ items
    dlg <- messageDialogNew (Just window) [] MessageQuestion
            ButtonsYesNo 
            (printf "Are you sure you want to delete macro %s?" itemstr)
    response <- dialogRun dlg
    widgetDestroy dlg
    case response of
         ResponseYes -> do
             mapM_ rmit (map fst items)
             disconnectMacro list model macdir  -- Remove bindings
             -- disconnectMacro will loadList itself
         _ -> return ()
    where rmit item = removeFile (macdir ++ "/" ++ item)

rename list model macdir xml = do
    items <- getSelectedItems list model
    let item = fst . head $ items

    renamewin <- xmlGetWidget xml castToWindow "renamew"
    entry <- xmlGetWidget xml castToEntry "rename-entry"
    okbt <- xmlGetWidget xml castToButton "renameOK"
    cancelbt <- xmlGetWidget xml castToButton "renameCancel"
    oldl <- xmlGetWidget xml castToLabel "rename-oldname"

    labelSetText oldl item

    widgetSetSensitivity okbt False
    onPasteClipboard entry (procentry okbt entry)
    entrySetText entry ""
    onKeyPress entry (\_ -> procentry okbt entry >> return False)
    onKeyRelease entry (\_ -> procentry okbt entry >> return False)

    onDelete renamewin (\_ -> cancel renamewin >> return True)
    onClicked okbt (renamedone renamewin entry item)
    onClicked cancelbt (cancel renamewin)
    windowPresent renamewin

    return ()

    where procentry okbt entry = do
              text <- entryGetText entry
              validity <- isvalid text
              widgetSetSensitivity okbt validity

          isvalidchar text = text /= "none" && text /= "" &&
                              (and . map (\c -> not $ c `elem` text) 
                               $ "/\0;{}[]|\\?()*&$!~`@")
          isvalid text = 
              if isvalidchar text
                 then do dfe <- doesFileExist (macdir ++ "/" ++ text)
                         dde <- doesDirectoryExist (macdir ++ "/" ++ text)
                         return ((not dfe) && (not dde))
                 else return False

          renamedone renamewin entry old = do
              text <- entryGetText entry
              validity <- isvalid text
              when (validity) $
                 renameFile (macdir ++ "/" ++ old) (macdir ++ "/" ++ text)
              widgetHide renamewin
              loadList list model macdir
                      
          cancel renamewin = widgetHide renamewin

record list model macdir xml = do
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
              loadList list model macdir

