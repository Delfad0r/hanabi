{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module BaseAI where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Function
import Data.List hiding (nub)
import Data.List.Ordered hiding (sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import Data.Tuple

import Types
import Utility

import Debug.Trace

data CardTag = NoTag | Trash
    deriving (Eq, Show)


data CardInfo = 
    CardInfo {
        _possibleCards :: [Card],
        _cardTag :: CardTag,
        _lastHinted :: Int
    }
    deriving (Show)
makeFieldsNoPrefix ''CardInfo


data PublicKnowledge =
    PublicKnowledge {
        _cardInfo :: [[CardInfo]],
        _virtualBoard :: Board,
        _currTurn :: Int
    }
    deriving (Show)
makeFieldsNoPrefix ''PublicKnowledge


data BaseAI =
    BaseAI {
        _game :: GameAppearance,
        _publicKnowledge :: PublicKnowledge
    }
makeFieldsNoPrefix ''BaseAI


instance Strategy BaseAI where

    initStrategy a =
        BaseAI {}
        & game .~ a
        & publicKnowledge .~ (initPublicKnowledge a)
    
    processEvent e a = do
        publicKnowledge %= updatePublicKnowledge a e
        game .= a
        when (a ^. myId == 0) $ logMessage $ show e
    
    playTurn a = do
        game .= a
        ai <- get
        let k = ai ^. publicKnowledge
            playableCards = map fst . filter (all (isPlayable (a ^. board)) . (^. possibleCards) . snd) . zip [0 ..] $ k ^. cardInfo . idx (a ^. myId)
            trashValueOfCards = [((ci ^. cardTag == Trash, i), i) | i <- [0 .. a ^. myHandSize - 1], let ci = k ^. cardInfo . idx (a ^. myId) . idx i]
        logMessage . unlines . map show . zip [0 ..] $ k ^. cardInfo . idx (a ^. myId)
        case playableCards of
            (i : _) -> do
                u <- use (publicKnowledge . cardInfo . idx (a ^. myId)) 
                return $ PlayAction {} & cardPos .~ i
            _ -> do
                let h = giveHintMod16 ai . sum . map (encodeHand k a) $ [0 .. numPlayers - 1] \\ [a ^. myId]
                if a ^. numHints > 0 && isJust h
                then return $ fromJust h
                else do 
                    u <- use (publicKnowledge . cardInfo . idx (a ^. myId)) 
                    return $ DiscardAction {} & cardPos .~ snd (maximum trashValueOfCards)
                
                    


initPublicKnowledge :: GameAppearance -> PublicKnowledge
initPublicKnowledge a =
    PublicKnowledge {}
    & cardInfo .~ (replicate numPlayers . replicate handSize $ CardInfo {} & possibleCards .~ (nubSort $ a ^. fullDeck) & cardTag .~ NoTag & lastHinted .~ 0)
    & virtualBoard .~ (a ^. board)
    & currTurn .~ 0


isValidHint :: Hint -> [Card] -> Bool
isValidHint h = not . null . filter (matchHintCard h)


getNumFromHintedCards :: Hint -> [Int] ->  Int
getNumFromHintedCards (ColorHint _) c
    | 0 `elem` c = 0
    | otherwise = 1
getNumFromHintedCards (NumberHint _) c
    | 0 `elem` c = 2
    | otherwise = 3


getNumFromHint :: Event -> Int
getNumFromHint e = getNumFromHintedCards (e ^?! hint) (e ^?! hintedCards)
    + 4 * head [n - 1 | n <- [1 ..], nextPlayerN n (e ^?! playerId) == e ^?! targetId]


giveHintMod16 :: BaseAI -> Int -> Maybe Action
--assumes numPlayers == 5, handSize == 4
giveHintMod16 ai n = do
        let l = filter (\h -> isValidHint h c && getNumFromHintedCards h (getHintedCards h c) == j) $ allHints
        listToMaybe l
        return
            $ HintAction {}
            & targetId .~ i
            & hint .~ (
                maximumBy (comparing $ \h -> ratePublicKnowledge . updatePublicKnowledge (ai ^. game) (
                    HintEvent {}
                    & playerId .~ (ai ^. game . myId)
                    & targetId .~ i
                    & hint .~ h
                    & hintedCards .~ (getHintedCards h c)
                ) $ ai ^. publicKnowledge) l
            )
    where
        (i, j) = ((n `mod` 16) `divMod` 4) & _1 %~ (nextPlayerN ?? 1 + ai ^. game . myId)
        c = ai ^. game . otherHands . idx i


encodeHand :: PublicKnowledge -> GameAppearance -> PlayerId -> Int
encodeHand k a i = 
    case listToMaybe . filter (isPlayable (k ^. virtualBoard) . snd) $ zip [0 .. numPlayable - 1] cardsForHint of
        Just (j, c) -> colNum * j + fromJust (elemIndex (c ^. color) cols)
        Nothing -> colNum * numPlayable + fromBase2 (map (isTrash $ k ^. virtualBoard) $ take numTrash cardsForHint)
    where
        cols = M.keys . M.filter (< 5) $ k ^. virtualBoard
        colNum = length cols
        numPlayable = cardsForHintNumPlayable !! colNum
        numTrash = cardsForHintNumTrash !! colNum
        cardsForHint = map (\j -> a ^. otherHands . idx i . idx j) $ selectCardsForHint k i

--                              0   1   2   3   4   5   6
cardsForHintNumPlayable =   [   0,  4,  4,  4,  3,  2,  2]
cardsForHintNumTrash =      [   4,  3,  3,  2,  2,  2,  2]

selectCardsForHint :: PublicKnowledge -> PlayerId -> [Int]
selectCardsForHint k i = sortOn (\j -> let ci = k ^. cardInfo . idx i . idx j in (ci ^. cardTag == Trash, ci ^. possibleCards . to length == 1, ci ^. lastHinted, -j)) [0 .. length (k ^. cardInfo . idx i) - 1]


ratePublicKnowledge :: PublicKnowledge -> Double
ratePublicKnowledge _ = 0


updatePublicKnowledge :: GameAppearance -> Event -> PublicKnowledge -> PublicKnowledge
updatePublicKnowledge a e k =
        updatePublicKnowledgeWithEvent e
        & deleteVisibleCards
        & updateVirtualBoard
        & knownCardsNotTrash
        & findTrashCards
    where
    unplayableHighCards = computeUnplayableHighCards a
    deleteVisibleCards k = k & cardInfo . mapped . mapped . possibleCards %~ (\case {[c] -> [c]; cs -> intersect cs $ computeInvisibleCards a k})
    knownCardsNotTrash k =
        k
        & cardInfo . mapped . mapped
            %~ (\ci -> case ci ^. possibleCards of {
                [c] -> ci & cardTag .~ (if isTrash (a ^. board) c || c `elem` unplayableHighCards then Trash else NoTag);
                _ -> ci
            })
    findTrashCards k =
        k
        & cardInfo . mapped . mapped
            %~ (\ci -> let pc = ci ^. possibleCards in if length pc > 1 && all (liftA2 (||) (isTrash $ k ^. virtualBoard) (`elem` unplayableHighCards)) pc then ci & cardTag .~ Trash else ci)
    updateVirtualBoard k = k
        & virtualBoard
            %~ fst
            . head
            . filter (uncurry (==))
            . (zip <*> tail)
            . iterate (imap $ \c n -> if [Card {} & number .~ n + 1 & color .~ c] `elem` (k ^.. cardInfo . traversed . traversed . possibleCards) then n + 1 else n)
        & virtualBoard %~ M.unionWith max (a ^. board)
    updatePublicKnowledgeWithEvent e@(DiscardEvent {..}) = k & currTurn %~ succ & cardInfo . idx (e ^. playerId) %~ deleteAt (e ^?! cardPos)
    updatePublicKnowledgeWithEvent e@(PlayEvent {..}) = k & currTurn %~ succ & cardInfo . idx (e ^. playerId) %~ deleteAt (e ^?! cardPos)
    updatePublicKnowledgeWithEvent e@(HintEvent {..}) =
        k
        & currTurn %~ succ
        & cardInfo .~ map updateOnePlayer [0 .. numPlayers - 1]
        & cardInfo . idx (e ^?! targetId) . elements (`elem` e ^?! hintedCards) . possibleCards %~ filter (matchHintCard $ e ^?! hint)
        & cardInfo . idx (e ^?! targetId) . elements (not . (`elem` e ^?! hintedCards)) . possibleCards %~ filter (not . (matchHintCard $ e ^?! hint))
        where
            n = getNumFromHint e
            updateOnePlayer i
                | i == e ^. playerId = k ^. cardInfo . idx i
                | n' < colNum * numPlayable =
                    let (num, col') = n' `divMod` colNum
                        col = cols !! col'
                    in k ^. cardInfo . idx i
                        & foldr (.) id [idx j %~ setNotPlayable | j <- take num cardsForHint]
                        & idx (cardsForHint !! num) %~ setPlayable col
                        & foldr (.) id [idx j . lastHinted .~ (k ^. currTurn + 1) | j <- take (num + 1) cardsForHint]
                | otherwise =
                    k ^. cardInfo . idx i
                    & elements (`elem` take numPlayable cardsForHint) %~ setNotPlayable
                    & elements (`elem` (map snd . filter fst . zip (toBase2 $ n' - colNum * numPlayable) $ take numTrash cardsForHint)) %~ setTrash
                    & elements (`elem` (map snd . filter (not . fst) . zip (toBase2 $ n' - colNum * numPlayable) $ take numTrash cardsForHint)) %~ setNotTrash
                    & foldr (.) id [idx j . lastHinted .~ (k ^. currTurn + 1) | j <- take (max numPlayable numTrash) cardsForHint]
                where
                    n'
                        | i == a ^. myId = (n - sum [encodeHand k a j | j <- [0 .. numPlayers - 1] \\ [i, e ^?! playerId]]) `mod` 16
                        | otherwise = encodeHand k a i
                    cardsForHint = selectCardsForHint k i
            setPlayable c ci = ci & possibleCards .~ [nextPlayableCard (k ^. virtualBoard) c]
            setNotPlayable ci = ci & possibleCards %~ (\\ map (nextPlayableCard $ k ^. virtualBoard) colors)
            setTrash ci = ci & possibleCards %~ (intersect $ M.foldMapWithKey (\c n -> [Card {} & number .~ m & color .~ c | m <- [1 .. n]]) (k ^. virtualBoard)) & cardTag .~ Trash
            setNotTrash ci = ci & possibleCards %~ (\\ M.foldMapWithKey (\c n -> [Card {} & number .~ m & color .~ c | m <- [1 .. n]]) (k ^. virtualBoard))
            cols = M.keys . M.filter (< 5) $ k ^. virtualBoard
            colNum = length cols
            numPlayable = cardsForHintNumPlayable !! colNum
            numTrash = cardsForHintNumTrash !! colNum
    updatePublicKnowledgeWithEvent e@(DrawEvent {..}) = updatePublicKnowledge a (YouDrawEvent {} & playerId .~ (e ^. playerId)) k
    updatePublicKnowledgeWithEvent e@(YouDrawEvent {..}) = k & cardInfo . idx (e ^. playerId) %~ ((CardInfo {} & possibleCards .~ computeInvisibleCards a k & cardTag .~ NoTag & lastHinted .~ 0) :)


isPlayable :: Board -> Card -> Bool
isPlayable b c = c ^. number - 1 == b ^. idx (c ^. color)


nextPlayableCard :: Board -> Color -> Card
nextPlayableCard b c = Card {} & number .~ 1 + (b ^. idx c) & color .~ c


isTrash :: Board -> Card -> Bool
isTrash b c = c ^. number <= b ^. idx (c ^. color)


computeInvisibleCards :: GameAppearance -> PublicKnowledge -> [Card]
computeInvisibleCards a k = nubSort $ (a ^. fullDeck \\ a ^. discardPile) \\ (map head . filter ((== 1) . length)  $ k ^.. cardInfo . traversed . traversed . possibleCards)


computeUnplayableHighCards :: GameAppearance -> [Card]
computeUnplayableHighCards a = nubSort . concatMap (\c -> [c & number .~ n | n <- [c ^. number .. 5]]) $ (nubSort $ a ^. fullDeck) `minus` (nubSort $ (a ^. fullDeck) `minus` nubSort (a ^. discardPile))


allHints :: [Hint]
allHints = map ColorHint colors ++ map NumberHint [1 .. numNumbers]


fromBase2 :: [Bool] -> Int
fromBase2 = foldr (flip $ (+) . (* 2)) 0 . map fromEnum

toBase2 :: Int -> [Bool]
toBase2 = map toEnum . unfoldr (Just . swap . (`divMod` 2))

