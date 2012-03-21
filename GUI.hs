module GUI where
import Graphics.UI.Gtk
import Data.Char (toUpper)

guiFunc :: IO ()
guiFunc = do
     initGUI
     window <- windowNew
     set window [windowTitle := "Ant Colony Simulation Parameters", windowDefaultWidth := 400,
                 windowDefaultHeight := 300 ]
     
     ntbk <- notebookNew
     containerAdd window ntbk
     set ntbk [notebookScrollable := False, notebookTabPos := PosTop]

     let names = ["Mode","Size","Run Simulation"]

     q <-hBoxNew True 15
     x <- frameNew
     frameSetLabel x "Yo!"
     --boxPackStart q x PackGrow 10
          
     w <-hBoxNew True 15
     y <- frameNew
     frameSetLabel y "Euch!"
     --boxPackStart w y PackGrow 10

     e <-hBoxNew True 15
     z <- frameNew
     frameSetLabel z "Blah!"
     --boxPackStart e z PackGrow 10

     notebookAppendPage ntbk x "Mode"
     notebookAppendPage ntbk y "Size"
     notebookAppendPage ntbk z "Run Simulation"

     
     

     onSwitchPage ntbk (putStrLn . ((++)"Page: ") . show)

     widgetShowAll window
     onDestroy window mainQuit
     mainGUI
       
