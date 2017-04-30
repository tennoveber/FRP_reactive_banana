
{-# LANGUAGE ScopedTypeVariables #-}
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

gui :: IO ()
gui = do
    f         <- frame [text := "Tekst ja nupp"]
    input     <- entry f []
    output    <- staticText f []
    but       <- button f [text := "Vajuta mind"]
    
    set f [layout := margin 10 $ row 10 [widget input
        , widget but, minsize (sz 70 20) $ widget output]]

    let networkDescription :: MomentIO ()
        networkDescription = do

            tekst <- behaviorText input ""
            but2  <- event0 but command

            (kaitumine :: Behavior String) 
                <- stepper "" (tekst <@ but2)

            sink output [text :== kaitumine]   

    network <- compile networkDescription    
    actuate network

main :: IO ()
main = start gui
