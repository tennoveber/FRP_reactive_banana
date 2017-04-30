
import Data.Maybe
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

gui :: IO ()
gui = 
    do
    f         <- frame [text := "Kahe arvu liitmine"]
    input1    <- entry f []
    input2    <- entry f []
    output    <- staticText f []
    
    set f [layout := margin 10 $ row 10
            [widget input1, label "+", widget input2
            , label "=", minsize (sz 40 20) $ widget output]]

    let networkDescription :: MomentIO ()
        networkDescription = do
        
        binput1  <- behaviorText input1 ""
        binput2  <- behaviorText input2 ""
        
        let
            readNumber :: String -> Maybe Int
            readNumber s = listToMaybe [x | (x,"") <- reads s]   

            bmayint1,bmayint2 :: Behavior (Maybe Int)
            bmayint1 = readNumber <$> binput1
            bmayint2 = readNumber <$> binput2

            addNumber :: Maybe Int -> Maybe Int -> Maybe Int
            addNumber (Just a) (Just b) = Just (a+b)
            addNumber _ _ = Nothing

            bsum :: Behavior (Maybe Int)
            bsum = addNumber <$> bmayint1 <*> bmayint2
             
            showNumber :: Maybe Int -> String
            showNumber (Just a) = show a
            showNumber Nothing = "--"


            result :: Behavior String
            result = showNumber <$> bsum
    
        sink output [text :== result]   

    network <- compile networkDescription    
    actuate network

main :: IO ()
main = start gui