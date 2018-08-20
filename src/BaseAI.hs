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

--TrashValue = 0 if I have two copies of the same card
--Rate card to play also based on visible (but not known) cards


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
            playableCards = reverse . map fst . sortOn (average . map (\c -> fromIntegral . subtract (c ^. number) . (^. idx (c ^. color)) . updateVirtualBoard pk . (idx (c ^. color) %~ succ) $ a ^. board) . snd) . filter (all (isPlayable $ a ^. board) . snd) . zip [0 ..] $ privateCardInfo ^.. idx i . traversed . possibleCards
            trashValues = memoizeCardFunction $ computeTrashValue a pk
            myKnownCards = sort [(j, c) | (i, [c]) <- imap (\j ci -> (j, ci ^. possibleCards)) $ privateCardInfo !! i, j <- [0 .. a ^. myHandSize], j /= i]
            trashValues' j c | (j, c) `member` myKnownCards = (0, 0)
            trashValues' j c = trashValues c
            trashValue j ci = case ci ^. possibleCards of
                    [c] | c ^. number <= 1 + pk ^. virtualBoard . idx (c ^. color) -> snd $ trashValues' j c
                    cs -> sum (map (fst . trashValues' j) cs) / genericLength cs
            trashValueOfCards = zip (imap trashValue $ privateCardInfo !! i) [0 ..]
        logMessage . unlines . zipWith (curry show) [0 ..] $ k ^. cardInfo . idx i
        if (a ^. turnsLeft . to isJust) then mapM_ (logMessage . unlines . zipWith (curry show) [0 ..]) privateCardInfo else logMessage . unlines . zipWith (curry show) [0 ..] $ privateCardInfo !! i
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
        & fixLastTurn
        & findFixedPointBy ((==) `on` (^. cardInfo)) prune
        & (\k' -> k' & virtualBoard .~ updateVirtualBoard k' (a ^. board))
        & updateVirtualTrash
    where
    prune k = k & cardInfo %~ imap (\i cis -> prunePossibleCards 64 ((a ^. fullDeck) `minus` (a ^. discardPile) `minus` playedCards (a ^. board) `minus` sort [c | [c] <- k ^.. cardInfo . traversed . indices (/= i) . traverse . possibleCards]) cis)
    updateVirtualTrash k = k & isVirtualTrash .~ memoizeCardList (computeTrashCards (k ^. virtualBoard) a)
    fixLastTurn :: PublicKnowledge -> PublicKnowledge
    fixLastTurn k | a ^. turnsLeft . to isJust =
        k
        & foldr (.) id (M.elems . imap (\i h -> cardInfo . idx i . mapped . possibleCards %~ isect (sort h)) $ a ^. otherHands)
        & cardInfo . idx (a ^. myId) . mapped . possibleCards %~ isect ((a ^. fullDeck) `minus` sort (a ^. discardPile) `minus` playedCards (a ^. board) `minus` sort (fold $ a ^. otherHands))
    fixLastTurn k = k


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
            | ps <- map (\j -> k ^. cardInfo . idx i . idx j . possibleCards) cardsForHint, product (map length ps) <= 16 = k ^. cardInfo . idx i & foldr (.) id (zipWith (\j c -> idx j . possibleCards .~ [c]) cardsForHint $ sequence ps !! n')
            -- | [j] <- cardsForHint, u <- k ^. cardInfo . idx i, u ^. idx j . possibleCards . to length <= 16 = u & idx j . possibleCards %~ pure . (!! n')
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
updatePublicKnowledgeWithEvent a e@YouDrawEvent {..} k = k & cardInfo . idx (e ^. playerId) %~ ((CardInfo {} & possibleCards .~ nub (computeInvisibleCards a (k ^. cardInfo)) & lastHinted .~ 0) :)


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
        ps | product (map length ps) <= 16 -> fromJust . elemIndex cardsForHint $ sequence ps
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
        s0 = fromIntegral ((highestScore ^. idx (c ^. color)) + 1 - (c ^. number)) / (5 ^ (remaining c - 1) * 2 ^ visibleAndKnown c)
        s1 = s0 / (2 ^ visible c)
        s2 = s0 * 0.9 ^ visible c
        isTrash = memoizeCardList $ computeTrashCards (a ^. board) a
        notDiscarded = (a ^. fullDeck) `minus` sort (a ^. discardPile)
        remaining = memoizeCardFreq notDiscarded
        highestScore = M.fromList [(c, length . takeWhile (== -1) . (zipWith (-) <*> tail) . (0 :) . nub . map (^. number) $ filter ((== c) . (^. color)) notDiscarded) | c <- colors]
        visible = memoizeCardFreq . fold $ a ^. otherHands
        visibleAndKnown = memoizeCardFreq [c | [c] <- k ^. cardInfo ^.. traversed . indices (/= (a ^. myId)) . traversed . possibleCards]


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
updateVirtualBoard k = findFixedPoint (imap $ \c n -> if [Card {} & number .~ n + 1 & color .~ c] `elem` (k ^.. cardInfo . traversed . traversed . possibleCards) then n + 1 else n)


computeTrashCards :: Board -> GameAppearance -> [Card]
computeTrashCards b a = sort $ M.foldMapWithKey (\c n -> [Card {} & number .~ m & color .~ c | m <- [1 .. n]]) b ++ computeUnplayableHighCards a


computeInvisibleCards :: GameAppearance -> [[CardInfo]] -> [Card]
computeInvisibleCards a k =
    (a ^. fullDeck)
    `minus` sort (a ^. discardPile)
    `minus` sort [c | [c] <- k ^.. traverse . traverse . possibleCards]
    `minus` playedCards (a ^. board)


computeUnplayableHighCards :: GameAppearance -> [Card]
computeUnplayableHighCards a = nubSort . concatMap (\c -> [c & number .~ n | n <- [c ^. number .. 5]]) $ nub (a ^. fullDeck) `minus` nub ((a ^. fullDeck) `minus` sort (a ^. discardPile))


computePrivateCardInfo :: GameAppearance -> PublicKnowledge -> [[CardInfo]]
computePrivateCardInfo a k = findFixedPoint prune $ k ^. cardInfo
    where
        i = a ^. myId
        prune = (\ciss -> ciss & elements (/= i) %@~ (\j -> prunePossibleCards 128 (invis `minus` sort (fold . M.delete j $ a ^. otherHands) `minus` sort [c | [c] <- ciss ^.. idx i . traversed . possibleCards]))) . (idx i %~ prunePossibleCards 128 (invis `minus` sort (fold $ a ^. otherHands)))
        invis = (a ^. fullDeck) `minus` sort (a ^. discardPile) `minus` playedCards (a ^. board)


prunePossibleCards :: Int -> [Card] -> [CardInfo] -> [CardInfo]
prunePossibleCards n invis cis = if product (cis' ^.. traversed . possibleCards . to length) <= n then cis' & foldr (.) id (imap (\i p -> idx i . possibleCards .~ p) ps'') else cis'
    where
        invis' = invis `minus` sort [c | [c] <- map (^. possibleCards) cis]
        cis' = cis <&> possibleCards %~ (\case [c] -> [c]; p -> isect p invis')
        ps'' = map nubSort . transpose $ computePossibleHands invis cis'


computePossibleHands :: [Card] -> [CardInfo] -> [[Card]]
computePossibleHands invis = filter (all (uncurry ((>=) . invisMemo) . (head &&& length)) . group . sort) . sequence . map (^. possibleCards)
    where
        invisMemo = memoizeCardFreq invis


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


average :: (Fractional a) => [a] -> a
average = liftA2 (/) sum genericLength

