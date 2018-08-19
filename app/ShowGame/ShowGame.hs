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
Wrong trash deduction: [B3,R1,B1,X1,W3,G3,B4,B2,R5,B1,G3,B5,X2,Y1,R1,G4,R4,W5,B1,Y3,Y1,Y5,B3,W4,Y1,G4,W1,Y4,R4,G1,B2,X4,Y2,X1,W4,G2,X3,W2,R1,R2,R2,Y3,X4,B4,G1,Y4,W1,W1,R3,G5,X2,G2,X3,G1,R3,Y2,X1,W2,W3,X5]
-}
