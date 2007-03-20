import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

main = do
    initGUI
    Just xml <- xmlNew "hellohaskell.glade"
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
