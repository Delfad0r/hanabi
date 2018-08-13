module Average where


import Control.Monad.Writer
import System.Random.Shuffle
import Control.Lens
import Control.Exception.Base

import Game
import BaseAI
import Types
import Deck
import Utility

playOneGame :: IO Int
playOneGame = do
    deck <- shuffleM standardDeck
    let (s, w) = runWriter (playGame deck :: Logger (GameState BaseAI))
    return . sum $ s ^. board

getAverage :: Int -> IO Double
getAverage nGames = do
    scores <- replicateM nGames playOneGame
    return $ (fromIntegral $ sum scores) / (fromIntegral nGames)
    
