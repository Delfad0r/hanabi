{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}


module Pretty where

import Control.Lens
import Control.Monad
import Data.List
import Data.List.Ordered
import qualified Data.Map.Strict as M
import Data.Ord
import qualified System.Console.ANSI as C
import qualified System.Console.Pretty as P

import Types
import BaseAI
import Utility


colToCol :: Color -> String -> String
colToCol White = P.color P.White
colToCol Blue = P.color P.Blue
colToCol Green = P.color P.Green
colToCol Yellow = P.color P.Yellow
colToCol Red = P.color P.Red
colToCol Rainbow = P.color P.Black


class PrettyShow a where
    prettyShow :: a -> String


instance PrettyShow Card where
    prettyShow c = colToCol (c ^. color) $ show c


instance PrettyShow [Card] where
    prettyShow [] = "  "
    prettyShow (c : _) = prettyShow c


instance PrettyShow Board where
    prettyShow = M.foldMapWithKey (\c n -> "  " ++ colToCol c (show n))


prettyPrintGameState :: GameState BaseAI -> IO ()
prettyPrintGameState s = do
    C.clearScreen
    C.setCursorPosition 0 0
    -- Player 0
    putStr . unwords $ map (showCard 0) [0 .. 3]
    -- Player 1
    C.cursorForward 4
    putStr . unwords $ map (showCard 1) [0 .. 3]
    -- Player 2
    C.cursorForward 4
    C.cursorDown 3
    sequence_ [putStr (showCard 2 j) >> C.cursorBackward 2 >> C.cursorDown 1 | j <- [0 .. 3]]
    -- Player 3
    C.cursorBackward 6
    C.cursorDown 2
    sequence_ [putStr (showCard 3 j) >> C.cursorBackward 5 | j <- [0 .. 3]]
    -- Player 4
    C.cursorBackward 3
    sequence_ [putStr (showCard 4 j) >> C.cursorBackward 5 | j <- [0 .. 3]]
    -- Board
    C.setCursorPosition 3 5
    putStr . prettyShow $ s ^. board
    -- Hints, Lives, Deck
    C.setCursorPosition 6 6
    putStr $ (show $ s ^. numHints) ++ "H " ++ (show $ s ^. numLives) ++ "L"
    C.cursorForward 3
    putStr . reverse . take 3 . ('D' :) . (++ " ") . reverse . show . length $ s ^. deck
    -- Discard Pile
    iforM_ colors $ \j c -> do
        let cards = flip (isectBy (comparing snd)) (map (0 ,) . sort $ s ^. discardPile) . zip [0 ..] . sort . filter ((== c) . (^. color)) $ s ^. fullDeck
        --Just (y, x) <- C.getCursorPosition
        forM_ cards $ \(i, card) -> C.setCursorPosition i (48 + 3 * j)  >> putStr (prettyShow card)
    -- Cursor
    C.setCursorPosition 11 0
    where
        showCard i j = maybe "  " (style (s ^. players . idx i . strategy . publicKnowledge . cardInfo . idx i . idx j) . prettyShow) $ s ^? players . idx i . hand . ix j
        style :: CardInfo -> String -> String
        style ci = (if 1 == length (ci ^. possibleCards) then P.style P.Bold else id) . (if Trash == ci ^. cardTag then P.style P.Italic else id)
