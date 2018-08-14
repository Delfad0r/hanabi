import Control.Monad.Writer
import System.Random.Shuffle
import Control.Lens
import Control.Exception.Base
import System.Environment
import Control.Parallel.Strategies

import Game
import BaseAI
import Types
import Deck
import Utility

playOneGame :: IO Int
playOneGame = do
    deck <- shuffleM standardDeck
    let s = fst $ runWriter (playGame deck :: Logger (GameState BaseAI))
    return . sum $ s ^. board

getAverage :: Int -> IO Double
getAverage nGames = do
    scores <- (`using` parList rdeepseq) <$> replicateM nGames playOneGame
    return $ fromIntegral (sum scores) / fromIntegral nGames

main :: IO ()
main = fmap (read . head) getArgs >>= getAverage >>= print
