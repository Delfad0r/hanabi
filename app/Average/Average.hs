import Control.Monad.Writer
import System.Random.Shuffle
import Control.Lens
import System.Environment
import Control.Parallel.Strategies
import Control.Exception
import System.Exit
import Data.List
import Text.Printf

import Game
import BaseAI
import Types
import Deck
import Utility

playOneGame :: IO Int
playOneGame = do
    deck <- shuffleM standardDeck
    let s = fst $ runWriter (playGame deck :: Logger (GameState BaseAI))
    --catch (return $! sum $ s ^. board) (\e -> do { print (e :: SomeException); print deck; exitWith (ExitFailure 1) })
    return . sum $ s ^. board

printAverage :: Int -> IO ()
printAverage nGames = do
    let maxScore = length $ nub standardDeck
    scores <- (`using` parListChunk 10 rdeepseq) <$> replicateM nGames playOneGame
    printf "average: %f\n" $ (fromIntegral (sum scores) / fromIntegral nGames :: Double)
    printf "perfect scores: %f%%\n" $ (100 * genericLength (filter (== maxScore) scores) / fromIntegral nGames :: Double)

main :: IO ()
main = fmap (read . head) getArgs >>= printAverage
