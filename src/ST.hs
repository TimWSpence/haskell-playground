{-# LANGUAGE FlexibleContexts #-}
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import Data.Hashable

quickSort :: Ord a => [a] -> [a]
quickSort items = runST $ do
    let len = length items
    arr <- newListArray (1, len) items
    quickSort' arr 1 len
    getElems arr
  where
    quickSort' :: Ord a => (STArray s Int a) -> Int -> Int -> ST s ()
    quickSort' arr low high = do
      when (low < high) $ do
        q <- partition arr low high
        quickSort' arr low (q - 1)
        quickSort' arr (q + 1) high

    partition arr low high = do
      i <- newSTRef (low - 1)
      pivot <- readArray arr high
      forM_ [low .. pred high] $ \j -> do
        jj <- readArray arr j
        when (jj <= pivot) $ do
          modifySTRef' i (+1)
          readSTRef i >>= \ii -> swap arr ii j
      ii <- readSTRef i
      swap arr (ii + 1) high
      return (ii + 1)

    swap arr i j = do
      v <- readArray arr i
      w <- readArray arr j
      writeArray arr i w
      writeArray arr j v

_nub :: (Hashable a, Eq a) => [a] -> [a]
_nub l = runST $ do
  arr <- mr
  forM_ l $ \j -> do
    let index = (hash j) `mod` 255
    current <- readArray arr index
    let new = if j `elem` current then current else j : current
    writeArray arr index new
  join <$> getElems arr
    where
      mr :: ST s (STArray s Int [a])
      mr = newListArray (0, 255) (replicate 256 [])


-- quickSort(arr[], low, high)
-- {
--     if (low < high)
--     {
--         /* pi is partitioning index, arr[pi] is now
--            at right place */
--         pi = partition(arr, low, high);

--         quickSort(arr, low, pi - 1);  // Before pi
--         quickSort(arr, pi + 1, high); // After pi
--     }
-- }

-- partition (arr[], low, high)
-- {
--     // pivot (Element to be placed at right position)
--     pivot = arr[high];  
 
--     i = (low - 1)  // Index of smaller element

--     for (j = low; j <= high- 1; j++)
--     {
--         // If current element is smaller than or
--         // equal to pivot
--         if (arr[j] <= pivot)
--         {
--             i++;    // increment index of smaller element
--             swap arr[i] and arr[j]
--         }
--     }
--     swap arr[i + 1] and arr[high])
--     return (i + 1)
-- }

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (h:t) = (qsort [x | x <- t, x <= h]) ++ [h] ++ (qsort [x | x <- t, x > h])
