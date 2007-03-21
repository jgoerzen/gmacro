{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

module Buttons where 
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

data Buttons = 
    Buttons {newbt :: Button,
             renamebt :: Button,
             removebt :: Button,
             connectbt :: Button,
             disconnectbt :: Button,
             aboutbt :: Button,
             closebt :: Button}

initButtons xml window = do
    newB <- get "newmacrobt"
    renameB <- get "renamebt"
    removeB <- get "removebt"
    connectB <- get "connectbt"
    disconnectB <- get "disconnectbt"
    aboutB <- get "aboutbt"
    closeB <- get "closebt"
    onClicked closeB (widgetDestroy window)
    let buttons = Buttons {newbt = newB, renamebt = renameB,
          removebt = removeB, connectbt = connectB,
          disconnectbt = disconnectB, aboutbt = aboutB, closebt = closeB}
    disablePerMacro buttons
    return buttons
    where get = xmlGetWidget xml castToButton 

disablePerMacro buttons =
    mapM_ ((flip widgetSetSensitivity) False) (macroButtons buttons)

enablePerMacro buttons =
    mapM_ ((flip widgetSetSensitivity) True) (macroButtons buttons)

macroButtons buttons =
    [renamebt buttons, removebt buttons, connectbt buttons,
     disconnectbt buttons]

