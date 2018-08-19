{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module BaseAI where


--Tell the exact hand if #possible hands <= 16 (considering only non-trash cards)
--Rate playable card by virtual board if played
--Prune with all combinations


import Control.Applicative
import Control.Arrow
import Control.Lens hiding (has)
import Control.Monad
import Control.Monad.State
import Data.Foldable (fold)
import Data.Function
import Data.List hiding (nub)
import Data.List.Ordered hiding (sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import Data.Tuple
import qualified Data.Vector as V

import ListFreq
import Types
import Utility


data CardInfo = 
    CardInfo {
        _possibleCards :: [Card],
        _lastHinted :: Int
    }
    deriving (Eq, Show)
makeFieldsNoPrefix ''CardInfo


data PublicKnowledge =
    PublicKnowledge {
        _cardInfo :: [[CardInfo]],
        _virtualBoard :: Board,
        _isVirtualTrash :: Card -> Bool,
        _currTurn :: Int
    }
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
        & publicKnowledge .~ initPublicKnowledge a
    
    processEvent e a = do
        publicKnowledge %= autoFixPublicKnowledge a . updatePublicKnowledgeWithEvent a e
        game .= a
        when (a ^. myId == 0) $ logMessage $ show e
    
    playTurn a = do
        game .= a
        ai <- get
        let k = ai ^. publicKnowledge
            i = a ^. myId
            privateCardInfo = computePrivateCardInfo a k
            pk = k & cardInfo .~ privateCardInfo & \k' -> k' & virtualBoard .~ updateVirtualBoard k' (a ^. board)
            playableCards = reverse . map fst . sortOn ((\case [c] -> pk ^. virtualBoard . idx (c ^. color) - c ^. number; _ -> -10) . snd) . filter (all (isPlayable $ a ^. board) . snd) . zip [0 ..] $ privateCardInfo ^.. idx i . traversed . possibleCards
            trashValues = memoizeCardFunction $ computeTrashValue a pk
            trashValue ci = case ci ^. possibleCards of
                    [c] | c ^. number <= 1 + pk ^. virtualBoard . idx (c ^. color) -> snd $ trashValues c
                    cs -> sum (map (fst . trashValues) cs) / genericLength cs
            trashValueOfCards = zip (map trashValue $ privateCardInfo !! i) [0 ..]
        logMessage . unlines . zipWith (curry show) [0 ..] $ k ^. cardInfo . idx i
        logMessage . unlines . zipWith (curry show) [0 ..] $ privateCardInfo !! i
        case playableCards of
            (j : _) -> return $ PlayAction {} & cardPos .~ j
            _ -> do
                let h = giveHintMod16 ai . sum . map (encodeHand k a) $ [0 .. numPlayers - 1] \\ [i]
                if a ^. numHints > 0 && isJust h
                then return $ fromJust h
                else do
                    logMessage $ show trashValueOfCards
                    return $ DiscardAction {} & cardPos .~ snd (minimum trashValueOfCards)
                

initPublicKnowledge :: GameAppearance -> PublicKnowledge
initPublicKnowledge a =
    PublicKnowledge {}
    & cardInfo .~ (replicate numPlayers . replicate handSize $ CardInfo {} & possibleCards .~ nubSort (a ^. fullDeck) & lastHinted .~ 0)
    & virtualBoard .~ (a ^. board)
    & isVirtualTrash .~ const False
    & currTurn .~ 0


autoFixPublicKnowledge :: GameAppearance -> PublicKnowledge -> PublicKnowledge
autoFixPublicKnowledge a k =
        k
        & findFixedPointBy ((==) `on` (^. cardInfo)) prune
        & (\k' -> k' & virtualBoard .~ updateVirtualBoard k' (a ^. board))
        & updateVirtualTrash
    where
    prune k = k & cardInfo %~ imap (\i cis -> prunePossibleCards ((a ^. fullDeck) `minus` (a ^. discardPile) `minus` (playedCards $ a ^. board) `minus` sort [c | [c] <- k ^.. cardInfo . traversed . indices (/= i) . traverse . possibleCards]) cis)
    --prune k = k & cardInfo . mapped . mapped . possibleCards %~ (\case {[c] -> [c]; cs -> intersect cs . computeInvisibleCards a $ k ^. cardInfo})
    updateVirtualTrash k = k & isVirtualTrash .~ memoizeCardList (computeTrashCards (k ^. virtualBoard) a)

updatePublicKnowledgeWithEvent :: GameAppearance -> Event -> PublicKnowledge -> PublicKnowledge
updatePublicKnowledgeWithEvent a e@DiscardEvent {..} k = k & currTurn %~ succ & cardInfo . idx (e ^. playerId) %~ deleteAt (e ^?! cardPos)
updatePublicKnowledgeWithEvent a e@PlayEvent {..} k = k & currTurn %~ succ & cardInfo . idx (e ^. playerId) %~ deleteAt (e ^?! cardPos)
updatePublicKnowledgeWithEvent a e@HintEvent {..} k =
    k
    & currTurn %~ succ
    & cardInfo .~ map updateOnePlayer [0 .. numPlayers - 1]
    & cardInfo . idx (e ^?! targetId) . elements (`elem` e ^?! hintedCards) . possibleCards %~ filter (matchHintCard $ e ^?! hint)
    & cardInfo . idx (e ^?! targetId) . elements (not . (`elem` e ^?! hintedCards)) . possibleCards %~ filter (not . matchHintCard (e ^?! hint))
    where
        n = getNumFromHint e
        updateOnePlayer i
            | i == e ^. playerId = k ^. cardInfo . idx i
            | null cardsForHint = k ^. cardInfo . idx i
            | [j] <- cardsForHint, u <- k ^. cardInfo . idx i, u ^. idx j . possibleCards . to length <= 16 = u & idx j . possibleCards %~ pure . (!! n')
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
        setNotPlayable ci = ci & possibleCards %~ (`minus` map (nextPlayableCard $ k ^. virtualBoard) colors)
        setTrash ci = ci & possibleCards %~ filter (k ^. isVirtualTrash)
        setNotTrash ci = ci & possibleCards %~ filter (not . (k ^. isVirtualTrash))
        cols = M.keys . M.filter (< 5) $ k ^. virtualBoard
        colNum = length cols
        numPlayable = cardsForHintNumPlayable !! colNum
        numTrash = cardsForHintNumTrash !! colNum
updatePublicKnowledgeWithEvent a e@DrawEvent {..} k = updatePublicKnowledgeWithEvent a (YouDrawEvent {} & playerId .~ (e ^. playerId)) k
updatePublicKnowledgeWithEvent a e@YouDrawEvent {..} k = k & cardInfo . idx (e ^. playerId) %~ ((CardInfo {} & possibleCards .~ computeInvisibleCards a (k ^. cardInfo) & lastHinted .~ 0) :)


isValidHint :: Hint -> [Card] -> Bool
isValidHint h = any (matchHintCard h)


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
giveHintMod16 ai n = do
        let l = filter (\h -> isValidHint h c && getNumFromHintedCards h (getHintedCards h c) == j) allHints
        listToMaybe l
        return
            $ HintAction {}
            & targetId .~ i
            & hint .~
                maximumBy (comparing $ \h -> ratePublicKnowledge . autoFixPublicKnowledge (ai ^. game) . updatePublicKnowledgeWithEvent (ai ^. game) (
                    HintEvent {}
                    & playerId .~ (ai ^. game . myId)
                    & targetId .~ i
                    & hint .~ h
                    & hintedCards .~ getHintedCards h c
                ) $ ai ^. publicKnowledge) l
    where
        (i, j) = ((n `mod` 16) `divMod` 4) & _1 %~ (nextPlayerN ?? 1 + ai ^. game . myId)
        c = ai ^. game . otherHands . idx i


encodeHand :: PublicKnowledge -> GameAppearance -> PlayerId -> Int
encodeHand k a i =
    case map (\j -> k ^. cardInfo . idx i . idx j . possibleCards) indicesForHint of
        [] -> 0
        [cs] | length cs <= 16 -> fromJust $ elemIndex (head cardsForHint) cs
        _ -> case listToMaybe . filter (isPlayable (k ^. virtualBoard) . snd) $ zip [0 .. numPlayable - 1] cardsForHint of
                Just (j, c) -> colNum * j + fromJust (elemIndex (c ^. color) cols)
                Nothing -> colNum * numPlayable + fromBase2 (map (k ^. isVirtualTrash) $ take numTrash cardsForHint)
    where
        cols = M.keys . M.filter (< 5) $ k ^. virtualBoard
        colNum = length cols
        numPlayable = cardsForHintNumPlayable !! colNum
        numTrash = cardsForHintNumTrash !! colNum
        indicesForHint = selectCardsForHint k i
        cardsForHint = map (\j -> a ^. otherHands . idx i . idx j) indicesForHint


--                              0   1   2   3   4   5   6
cardsForHintNumPlayable =   [   0,  4,  4,  4,  3,  2,  2]
cardsForHintNumTrash =      [   4,  3,  3,  2,  2,  2,  2]

selectCardsForHint :: PublicKnowledge -> PlayerId -> [Int]
selectCardsForHint k i = map (negate . snd) $ sort [(ci ^. lastHinted, -j) | j <- [0 .. length (k ^. cardInfo . idx i) - 1], let ci = k ^. cardInfo . idx i . idx j, any (not . (k ^. isVirtualTrash)) $ ci ^. possibleCards, ci ^. possibleCards . to length > 1]


ratePublicKnowledge :: PublicKnowledge -> Double
ratePublicKnowledge _ = 0


computeTrashValue :: GameAppearance -> PublicKnowledge -> Card -> (Double, Double)
computeTrashValue a k c
    | isTrash c = (0, 0)
    | k ^. isVirtualTrash $ c = (0, s2)
    | otherwise = (s1, s2)
    where
        s0 = fromIntegral ((highestScore ^. idx (c ^. color)) + 1 - (c ^. number)) / fromIntegral (5 ^ (remaining c - 1) * 2 ^ visibleAndKnown c)
        s1 = s0 / fromIntegral (2 ^ visible c)
        s2 = s0 * 0.9 ^ visible c
        isTrash = memoizeCardList $ computeTrashCards (a ^. board) a
        notDiscarded = (a ^. fullDeck) `minus` sort (a ^. discardPile)
        remaining = memoizeCardFreq notDiscarded
        highestScore = M.fromList [(c, length . takeWhile (== -1) . (zipWith (-) <*> tail) . (0 :) . nub . map (^. number) $ filter ((== c) . (^. color)) notDiscarded) | c <- colors]
        visible = memoizeCardFreq . fold $ a ^. otherHands
        visibleAndKnown = memoizeCardFreq [c | [c] <- (k ^. cardInfo & deleteAt (a ^. myId)) ^.. traversed . traversed . possibleCards]


cards :: [Card]
cards = [Card {} & color .~ c & number .~ n | c <- colors, n <- [1 .. numNumbers]]


cardToInt :: Card -> Int
cardToInt c = (c ^. color . to fromEnum) * numNumbers + c ^. number - 1


memoizeCardFunction :: (Card -> a) -> Card -> a
memoizeCardFunction f = (v V.!) . cardToInt
    where
        v = V.accum (flip const) (V.replicate (numColors * numNumbers) undefined) $ map (cardToInt &&& f) cards


memoizeCardFreq :: [Card] -> Card -> Int
memoizeCardFreq cs = (v V.!) . cardToInt
    where
        v = V.accum (+) (V.replicate (numColors * numNumbers) 0) $ map ((, 1) . cardToInt) cs


memoizeCardList :: [Card] -> Card -> Bool
memoizeCardList cs = (v V.!) . cardToInt
    where
        v = V.accum (flip const) (V.replicate (numColors * numNumbers) False) $ map ((, True) . cardToInt) cs


isPlayable :: Board -> Card -> Bool
isPlayable b c = c ^. number - 1 == b ^. idx (c ^. color)


nextPlayableCard :: Board -> Color -> Card
nextPlayableCard b c = Card {} & number .~ 1 + (b ^. idx c) & color .~ c


playedCards :: Board -> [Card]
playedCards = M.foldMapWithKey (\c n -> [Card {} & color .~ c & number .~ m | m <- [1 .. n]])


updateVirtualBoard :: PublicKnowledge -> Board -> Board
updateVirtualBoard k b = findFixedPoint (imap $ \c n -> if [Card {} & number .~ n + 1 & color .~ c] `elem` (k ^.. cardInfo . traversed . traversed . possibleCards) then n + 1 else n) b


computeTrashCards :: Board -> GameAppearance -> [Card]
computeTrashCards b a = sort $ M.foldMapWithKey (\c n -> [Card {} & number .~ m & color .~ c | m <- [1 .. n]]) b ++ computeUnplayableHighCards a


computeInvisibleCards :: GameAppearance -> [[CardInfo]] -> [Card]
computeInvisibleCards a k =
    nub
    $ (a ^. fullDeck)
    `minus` sort (a ^. discardPile)
    `minus` sort [c | [c] <- k ^.. traverse . traverse . possibleCards]
    `minus` playedCards (a ^. board)


computeUnplayableHighCards :: GameAppearance -> [Card]
computeUnplayableHighCards a = nubSort . concatMap (\c -> [c & number .~ n | n <- [c ^. number .. 5]]) $ nub (a ^. fullDeck) `minus` nub ((a ^. fullDeck) `minus` sort (a ^. discardPile))


computePrivateCardInfo :: GameAppearance -> PublicKnowledge -> [[CardInfo]]
computePrivateCardInfo a k = findFixedPoint prune $ k ^. cardInfo
    where
        i = a ^. myId
        prune = (\ciss -> ciss & elements (/= i) %@~ (\j -> prunePossibleCards (invis `minus` sort (fold . M.delete j $ a ^. otherHands) `minus` sort [c | [c] <- ciss ^.. idx i . traversed . possibleCards]))) . (idx i %~ prunePossibleCards (invis `minus` sort (fold $ a ^. otherHands)))
        invis = (a ^. fullDeck) `minus` sort (a ^. discardPile) `minus` playedCards (a ^. board)


prunePossibleCards :: [Card] -> [CardInfo] -> [CardInfo]
prunePossibleCards invis cis = cis & mapped . possibleCards %~ (\case [c] -> [c]; p -> isect p invis')
    where
        invis' = invis `minus` sort [c | [c] <- map (^. possibleCards) cis]


allHints :: [Hint]
allHints = map ColorHint colors ++ map NumberHint [1 .. numNumbers]


fromBase2 :: [Bool] -> Int
fromBase2 = foldr (flip ((+) . (* 2)) . fromEnum) 0

toBase2 :: Int -> [Bool]
toBase2 = map toEnum . unfoldr (Just . swap . (`divMod` 2))


findFixedPoint :: (Eq a) => (a -> a) -> a -> a
findFixedPoint = findFixedPointBy (==)

findFixedPointBy :: (a -> a -> Bool) -> (a -> a) -> a -> a
findFixedPointBy eq f = fst . head . filter (uncurry eq) . (zip <*> tail) . iterate f

