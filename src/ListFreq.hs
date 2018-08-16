{-# LANGUAGE TupleSections #-}


module ListFreq
( ListFreq, toList
, fromList, fromDiscreteList, fromSortedList
, fromListWithFreq, fromDiscreteListWithFreq, fromSortedListWithFreq
, norm, normSorted
) where


import Control.Applicative
import Control.Arrow
import Control.Monad
import qualified Data.Foldable as F
import Data.Function
import Data.List hiding (nub)
import Data.List.Ordered hiding (sort, sortBy, sortOn)


newtype ListFreq a = ListFreq {toList :: [(a, Int)]}

instance (Show a) => Show (ListFreq a) where
    show l = "fromList " ++ show (toList l)

instance Functor ListFreq where
    fmap f = ListFreq . map (first f) . toList

instance Applicative ListFreq where
    pure x = ListFreq [(x, 1)]
    f <*> l = ListFreq $ map (\(g, k) -> g *** (* k)) (toList f) <*> toList l

instance Monad ListFreq where
    l >>= f = ListFreq $ toList l >>= \(x, h) -> map (second (* h)) . toList $ f x

instance Alternative ListFreq where
    empty = mempty
    (<|>) = (<>)

instance MonadPlus ListFreq

instance Foldable ListFreq where
    foldMap f = foldr (mappend . mconcat . uncurry (flip replicate) . first f) mempty . toList
    null = null . toList
    length = sum . map snd . toList
    elem a = elem a . map fst . toList
    maximum = maximum . map fst . toList
    minimum = minimum . map fst . toList
    sum = sum . map (uncurry (*) . second fromIntegral) . toList
    product = product . map (uncurry (^)) . toList

instance Traversable ListFreq where
    sequenceA = fmap fromDiscreteList . sequenceA . F.toList 

instance Semigroup (ListFreq a) where
    x <> y = ListFreq $ toList x <> toList y

instance Monoid (ListFreq a) where
    mempty = ListFreq []


fromList :: (Eq a) => [a] -> ListFreq a
fromList = fromListWithFreq . map (, 1)

fromDiscreteList :: [a] -> ListFreq a
fromDiscreteList = ListFreq . map (, 1)

fromSortedList :: (Eq a) => [a] -> ListFreq a
fromSortedList = ListFreq . map (head &&& length) . group


fromListWithFreq :: (Eq a) => [(a, Int)] -> ListFreq a
fromListWithFreq = ListFreq . foldr f []
    where
        f u [] = [u]
        f (a, m) ((x, n) : xs)
            | x == a = (x, m + n) : xs
            | otherwise = (x, n) : f (a, m) xs

fromDiscreteListWithFreq :: [(a, Int)] -> ListFreq a
fromDiscreteListWithFreq = ListFreq

fromSortedListWithFreq :: (Eq a) => [(a, Int)] -> ListFreq a
fromSortedListWithFreq = ListFreq . map ((head *** sum) . unzip) . groupBy ((==) `on` fst) 


norm :: (Ord a) => ListFreq a -> ListFreq a
norm = normSorted . ListFreq . sortOn fst . toList 

normSorted :: (Eq a) => ListFreq a -> ListFreq a
normSorted = fromSortedListWithFreq . toList
