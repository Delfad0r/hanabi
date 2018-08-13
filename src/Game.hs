{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Game where

import Control.Applicative
import Control.Lens
import Control.Monad.Loops
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe

import Deck
import Types
import Utility


type GameAction s = StateT (GameState s) (Writer [String])
type MaybeGameAction s = ExceptT String (GameAction s)


instance Strategy () where
    initStrategy _ = ()
    processEvent _ _ = return ()
    playTurn _ = return $ PlayAction {} & cardPos .~ 0


playGame :: (Strategy s) => Deck -> Logger (GameState s)
playGame = playTracedGame $ return ()


playTracedGame :: (Strategy s) => LoggedState (GameState s) () -> Deck -> Logger (GameState s)
playTracedGame tr d = execStateT ?? gameState0 $ tr >> (processTurn tr `untilM_` use gameOver)
    where
        (hands, d') = splitAt (numPlayers * handSize) d
            & _1 %~ takeWhile (not . null) . unfoldr (Just . splitAt handSize)
        ps = map (\(i, h) -> initPlayer h $ toAppearance i gameState0) $ zip [0 ..] hands
        gameState0 =
            GameState {}
            & deck .~ d'
            & fullDeck .~ d
            & discardPile .~ []
            & board .~ M.fromList (map (, 0) colors)
            & numHints .~ maxHints
            & numLives .~ maxLives
            & currentPlayer .~ 0
            & players .~ ps
            & gameOver .~ False
            & gameOverBecause .~ ""
            & turnsLeft .~ Nothing


processTurn :: (Strategy s) => LoggedState (GameState s) () -> LoggedState (GameState s) ()
processTurn tr = do
    res <- runExceptT $ do
        i <- use currentPlayer
        a <- lift $ (gets $ toAppearance i) >>= zoom (players . idx i . strategy) . playTurn
        case a of
            PlayAction {..} -> do
                mc <- preuse $ players . idx i . hand . (ix $ a ^?! cardPos)
                case mc of
                    Nothing -> throwError "Invalid card position"
                    Just c -> do
                        players . idx i . hand %= deleteAt (a ^?! cardPos)
                        s <- uses (board . (idx $ c ^. color)) $ (== c ^. number) . succ
                        if s then do
                            board . (idx $ c ^. color) %= succ
                            when (c ^. number == 5) $ numHints %= (min maxHints) . succ
                        else do
                            discardPile %= (c :)
                            numLives -= 1
                        lift . forM_ [0 .. numPlayers - 1] . dispatchEvent
                            $ PlayEvent {}
                            & playerId .~ i
                            & card .~ c
                            & cardPos .~ (a ^?! cardPos)
                            & success .~ s
                        whenM (uses board $ all (== 5)) $ throwError "Victory"
                        drawCard i
            DiscardAction {..} -> do
                mc <- preuse $ players . idx i . hand . (ix $ a ^?! cardPos)
                case mc of
                    Nothing -> throwError "Invalid card position"
                    Just c -> do
                        players . idx i . hand %= deleteAt (a ^?! cardPos)
                        discardPile %= (c :)
                        numHints %= (min maxHints) . succ
                        lift . forM_ [0 .. numPlayers - 1] . dispatchEvent
                            $ DiscardEvent {}
                            & playerId .~ i
                            & card .~ c
                            & cardPos .~ (a ^?! cardPos)
                        drawCard i
            HintAction {..} -> do
                whenM (uses numHints (== 0)) $ throwError "No available hint tokens"
                p <- uses (players . idx (a ^?! targetId) . hand) . getHintedCards $ a ^?! hint
                when (i == a ^?! targetId || null p) $ throwError "Invalid hint"
                numHints -= 1
                lift . forM_ [0 .. numPlayers - 1] . dispatchEvent
                    $ HintEvent {}
                    & playerId .~ i
                    & targetId .~ (a ^?! targetId)
                    & hint .~ (a ^?! hint)
                    & hintedCards .~ p
        turnsLeft %= fmap pred
        whenM (liftM2 (&&) (uses deck null) (uses turnsLeft isNothing)) $ turnsLeft .= Just numPlayers
        whenM (uses turnsLeft (== Just 0)) $ throwError "Last turn"
        whenM (uses numLives (== 0)) $ throwError "No more lives"
        currentPlayer %= nextPlayer
    case res of
        Left e -> do
            gameOver .= True
            gameOverBecause .= e
        _ -> return ()
    tr


dispatchEvent :: (Strategy s) => Event -> PlayerId -> LoggedState (GameState s) ()
dispatchEvent e i = (gets $ toAppearance i) >>= zoom (players . idx i . strategy) . processEvent e


drawCard :: (Strategy s) => PlayerId -> ExceptT String (LoggedState (GameState s)) ()
drawCard i = do
    d <- use deck
    case d of
        [] -> return ()
        (c : d') -> do
            deck .= d'
            players . idx i . hand %= (c :)
            lift $ dispatchEvent ?? i $ YouDrawEvent {} & playerId .~ i
            lift . forM_ ([0 .. numPlayers - 1] \\ [i]) . dispatchEvent
                $ DrawEvent {}
                & playerId .~ i
                & card .~ c


initPlayer :: (Strategy s) => [Card] -> GameAppearance -> Player s
initPlayer h a =
    Player {}
    & playerId .~ (a ^. myId)
    & hand .~ h
    & strategy .~ initStrategy a


toAppearance :: PlayerId -> GameState s -> GameAppearance
toAppearance i s =
    GameAppearance {}
    & myId .~ i
    & fullDeck .~ sort (s ^. fullDeck)
    & deckSize .~ (s ^. deck . to length)
    & discardPile .~ (s ^. discardPile)
    & board .~ (s ^. board)
    & numHints .~ (s ^. numHints)
    & numLives .~ (s ^. numLives)
    & currentPlayer .~ (s ^. currentPlayer)
    & myHandSize .~ (s ^. players . idx i . hand . to length)
    & otherHands .~ (M.delete i . M.fromList . zip [0 ..] $ s ^. players ^.. traverse . hand)
    & gameOver .~ (s ^. gameOver)
    & turnsLeft .~ (s ^. turnsLeft)
