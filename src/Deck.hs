{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Deck where

import Control.Lens
import Data.List

import Types


standardDeck :: Deck
standardDeck = [Card {} & color .~ c & number .~ n | c <- colors \\ [Rainbow], n <- [1, 1, 1, 2, 2, 3, 3, 4, 4, 5]]
    ++ [Card {} & color .~ Rainbow & number .~ n | n <- numbers]
