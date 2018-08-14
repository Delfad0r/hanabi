import Control.Monad.Writer
import System.Random.Shuffle
import System.IO
import Control.Lens
import Control.Exception.Base
import Control.Monad.State
import System.Environment
import System.Exit

import Game
import BaseAI
import Types
import Deck
import Utility
import Pretty

main :: IO ()
main = do
    args <- getArgs
    deck <- case args of
        [d] -> return $ read d
        [] -> shuffleM standardDeck
        _ -> exitWith $ ExitFailure 1
    hSetBuffering stdin NoBuffering
    let tr = get >>= \s -> tell (prettyPrintGameState s >> getLine >> return ())
    let (s, w) = runWriter (playTracedGame tr deck :: Logger (GameState BaseAI))
    print . sum $ s ^. board
    getLine
    w
    print $ s ^. fullDeck
    print $ s ^. discardPile
    print $ s ^. players ^.. traversed . hand
    print $ s ^. board
    print . sum $ s ^. board
    print $ s ^. players . idx 0 . strategy . publicKnowledge . virtualBoard
    putStrLn $ s ^. gameOverBecause
    assert (s ^. numLives == 3) $ return ()

{- DISASTER
23 : [W1,R5,B2,Y3,B5,Y1,G1,X5,B4,Y2,G3,Y1,G2,B1,G4,R3,X2,B4,W1,X4,Y3,W4,R3,Y1,W4,W5,R4,R2,Y2,G4,G1,X3,Y4,B1,B2,X1,G1,G3,R2,G2,B1,W3,G5,W3,R1,R1,Y5,Y4,B3,R1,B3,R4,W2,W1,W2]
26 : [W1,G1,W4,B1,W3,B2,B5,Y3,G1,B1,G3,W4,B4,Y4,R2,Y4,Y3,G5,G1,Y1,B3,B3,X2,X5,G4,W1,B4,Y5,X1,R5,R3,R4,W1,G3,G2,W3,R2,R1,Y1,X4,Y2,B2,W2,Y2,B1,X3,G2,R1,R3,R4,W2,G4,R1,W5,Y1]
25 : [W4,R3,B4,W3,W3,W2,B3,Y3,B2,R4,G4,Y4,B2,R2,B1,B5,Y2,X4,G5,G3,R4,X1,Y2,W2,W1,W1,G1,W5,G2,G3,X5,B3,R2,Y3,B1,G1,X3,R1,B1,G4,B4,G1,R1,Y5,W4,R5,G2,Y1,R3,X2,R1,Y4,W1,Y1,Y1]
-}
