{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

module Actions where 
import Graphics.UI.Gtk
import Buttons
import MacroList

initActions b list model window = do
    onClicked (closebt b) (widgetDestroy window)

