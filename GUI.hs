--GUI

module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore 

main :: IO ()
main
  = start gui

gui :: IO ()
gui
  = do -- Main GUI Elements: Frame, Panel, Text control, and the Notebook
       f       <- frame [text := "Ant Simulation Parameters"]
       p       <- panel f []
       nb      <- notebook p [] 
       textlog <- textCtrl p [enabled := False, wrap := WrapNone] 

       -- use text control as logger
       textCtrlMakeLogActiveTarget textlog
       logMessage "Logging enabled"              
       -- set f [on closing :~ \prev -> do logSetActiveTarget oldlog; logDelete log; prev]

       -- radio box page
       p2   <- panel  nb []
       let fstLabels = ["Search", "Colonate", "Random"]
       let sndLabels = ["Parallel", "Concurrent", "Not Concurrent"]
       r1   <- radioBox p2 Vertical fstLabels   [text := "Ant Behaviours", on select ::= logSelect]
       r2   <- radioBox p2 Vertical sndLabels   [text := "Backend Behaviours", on select ::= logSelect] --tooltip String property
       rb1  <- button   p2 [text := "Disable", on command ::= onEnable r1]

       -- slider/gauge page
       p3   <- panel nb []
       s    <- hslider p3 True {- show labels -} 1 100 [selection := 50]
       g    <- hgauge  p3 100 [selection := 50]
       set s [on command := do{ i <- get s selection; logMessage("Ants starting " ++ (show i)); set g [selection := i] } ]


       -- button page
       p1   <- panel  nb []
       ok   <- button p1 [text := "Run Simulation", on command := do{logMessage "Start Simulation"; getAllValues s r1 r2 rb1}]
       quit <- button p1 [text := "Quit", on command := close f]


       -- specify layout
       set f [layout :=
                container p $
                column 0
                 [ tabs nb
                    [tab "Main" $ 
                     container p1 $ margin 15 $ floatCentre $ row 5 [widget ok, widget quit]

                    ,tab "Selections" $ 
                     container p2 $ margin 15 $ column 5 [ hstretch $ widget rb1
                                                         , row 0 [floatLeft $ widget r1
                                                         ,floatRight $ widget r2]
                                                         ]

                    ,tab "Ants" $ 
                     container p3 $ margin 15 $ column 5 [ hfill $ widget s
                                                         , hfill $ widget g
                                                         , glue
                                                         ]
                    ]
                 , hfill $ minsize (sz 20 80) $ widget textlog
                 ]
             , clientSize := sz 400 300 ]
       return ()

  where
    -- logSelect :: (Selection w, Items w String) => w -> IO ()
    logSelect w
      = do i <- get w selection
           s <- get w (item i)
           logMessage ("selected index: " ++ show i ++ ": " ++ s)
           
    onEnable w b
      = do set w [enabled :~ not]
           enable <- get w enabled
           set b [text := (if enable then "Disable" else "Enable")]

getAllValues s r1 r2 rb1 = do
                                i<-get s selection
                                putStrLn("woot " ++ show i)
                                -- pass To Simulation
