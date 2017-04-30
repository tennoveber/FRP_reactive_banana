{-# LANGUAGE ScopedTypeVariables #-}
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

gui :: IO ()
gui = do
    f  <- frame [ text       := "Hiire liikumine"
                , resizeable := False ]
    t  <- timer f [ interval := 10 ]    
    p  <- panel f []
    set f [ layout  := minsize (sz 300 300) $ widget p]
    
    let networkDescription :: MomentIO ()
        networkDescription = do
            taimerE  <- event0 t command
            hiirE    <- event1 p mouse  
            
            (bcolor :: Behavior Color) <- 
                stepper red (varv <$> (filterJust (justMotion <$> hiirE)))
            
            sink f [bgcolor :== bcolor]
            reactimate $ repaint f <$ taimerE

    network <- compile networkDescription    
    actuate network

main :: IO ()
main = start gui

varv :: Point -> Color
varv x = if (pointX x > 150) && (pointY x > 150) then yellow 
    else if (pointX x > 150) && (pointY x < 150) then green
    else if (pointX x < 150) && (pointY x > 150) then red  
    else blue

justMotion :: EventMouse -> Maybe Point
justMotion (MouseMotion pt _) = Just pt
justMotion _                  = Nothing
