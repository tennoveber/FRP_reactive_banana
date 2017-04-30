{-# LANGUAGE ScopedTypeVariables #-}

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

gui :: IO ()
gui = 
    do
    f         <- frame [text := "Nupuvajutus"]
    nupp    <- button f [text := "+1"]
    valjund    <- staticText f []
    valjund2   <- staticText f []
    set f [layout := minsize (sz 350 40) $ margin 10 $ row 10 [widget nupp
        , label " Summa: ", widget valjund, widget valjund2]]

    let networkDescription :: MomentIO ()
        networkDescription = do
            nuppE   <- event0 nupp   command
            (behavior1 :: Behavior Int) <- accumB 0 ((+1) <$ nuppE)

            let
                behavior2 :: Behavior String
                behavior2 = f <$> behavior1
                    where
                    f x = if even x then "Tegemist on paaris arvuga" 
                          else "Tegemist on paaritu arvuga"
            sink valjund [text :==  show <$> behavior1 ]
            sink valjund2 [text :==  show <$> behavior2 ]
        

    network <- compile networkDescription
    actuate network
main :: IO ()
main = start gui