{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as M


type Logger = Writer (IO ())
type LoggedState s = StateT s Logger


numColors = 6 :: Int
numNumbers = 5 :: Int
maxHints = 8 :: Int
maxLives = 3 :: Int
numPlayers = 5 :: Int
handSize = 4 :: Int

colors = map toEnum [0 .. numColors - 1] :: [Color]
numbers = [1 .. numNumbers] :: [Int]


data Color = White | Blue | Green | Yellow | Red | Rainbow
   deriving (Eq, Ord, Enum)

instance Show Color where
    show White = "W"
    show Blue = "B"
    show Green = "G"
    show Yellow = "Y"
    show Red = "R"
    show Rainbow = "X"

instance Read Color where
    readsPrec _ (c : rest) = take 1 [(col, rest) | col <- colors, show col == [c]]


data Card =
    Card {
        _color :: Color,
        _number :: Int
    }
    deriving (Eq, Ord)
makeFieldsNoPrefix ''Card;

instance Show Card where
    show c = show (c ^. color) ++ show (c ^. number)

instance Read Card where
    readsPrec _ (c : n : rest) = [(Card {} & color .~ read [c] & number .~ read [n], rest)]


type Deck = [Card]

type PlayerId = Int

type Board = M.Map Color Int


data Hint = ColorHint Color | NumberHint Int
    deriving (Show)
    

data Action =
    PlayAction {
        _cardPos :: Int
    } |
    DiscardAction {
        _cardPos :: Int
    } |
    HintAction {
        _targetId :: PlayerId,
        _hint :: Hint
    }
    deriving (Show)
makeFieldsNoPrefix ''Action


data Event =
    PlayEvent {
        _playerId :: PlayerId,
        _card :: Card,
        _cardPos :: Int,
        _success :: Bool
    } |
    DiscardEvent {
        _playerId :: PlayerId,
        _card :: Card,
        _cardPos :: Int
    } |
    HintEvent {
        _playerId :: PlayerId,
        _targetId :: PlayerId,
        _hint :: Hint,
        _hintedCards :: [Int]
    } |
    DrawEvent {
        _playerId :: PlayerId,
        _card :: Card
    } |
    YouDrawEvent {
        _playerId :: PlayerId
    }
    deriving (Show)
makeFieldsNoPrefix ''Event


data GameAppearance =
    GameAppearance {
        _myId :: PlayerId,
        _fullDeck :: Deck,
        _deckSize :: Int,
        _discardPile :: [Card],
        _board :: Board,
        _numHints :: Int,
        _numLives :: Int,
        _currentPlayer :: PlayerId,
        _myHandSize :: Int,
        _otherHands :: M.Map PlayerId [Card],
        _gameOver :: Bool,
        _turnsLeft :: Maybe Int
    }
    deriving (Show)
makeFieldsNoPrefix ''GameAppearance


class Strategy s where
    initStrategy :: GameAppearance -> s
    processEvent :: Event -> GameAppearance -> LoggedState s ()
    playTurn :: GameAppearance -> LoggedState s Action


data Player s =
    Player {
        _playerId :: PlayerId,
        _hand :: [Card],
        _strategy :: s
    }
    deriving (Show)
makeFieldsNoPrefix ''Player


data GameState s =
    GameState {
        _deck :: Deck,
        _fullDeck :: Deck,
        _discardPile :: [Card],
        _board :: Board,
        _numHints :: Int,
        _numLives :: Int,
        _currentPlayer :: PlayerId,
        _players :: [Player s],
        _gameOver :: Bool,
        _gameOverBecause :: String,
        _turnsLeft :: Maybe Int
    }
    deriving (Show)
makeFieldsNoPrefix ''GameState
