{-# LANGUAGE FlexibleContexts #-}

module Utility where

import Control.Lens
import Control.Monad.Writer

import Types


logMessage :: (MonadWriter (IO ()) m) => String -> m ()
logMessage = tell . putStrLn

nextPlayer :: PlayerId -> PlayerId
nextPlayer = (`mod` numPlayers) . succ

nextPlayerN :: Int -> PlayerId -> PlayerId
nextPlayerN n = (iterate (nextPlayer .) id !! n) . (`mod` numPlayers)

matchHintCard :: Hint -> Card -> Bool
matchHintCard (ColorHint col) card = col == card ^. color
matchHintCard (NumberHint num) card = num == card ^. number

getHintedCards :: Hint -> [Card] -> [Int]
getHintedCards h = map fst . filter (matchHintCard h . snd) . zip [0 ..]

deleteAt :: Int -> [a] -> [a]
deleteAt i = (++) <$> take i <*> drop (i + 1)

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond t = cond >>= (`when` t)

idx :: (Ixed t, Functor f) => Index t -> Over (->) f t t (IxValue t) (IxValue t)
idx = singular . ix
