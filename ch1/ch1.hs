import Data.List
import Data.Array
import qualified Data.Vector as V
import Test.QuickCheck


-- simple, nice but O(n^2)
minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

vsearch :: V.Vector Bool -> Int
vsearch = V.length . V.takeWhile id

-- pure functional
checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) $ zip (filter ( <= n) xs) (repeat True)
    where n = length xs

vchecklist :: [Int] -> V.Vector Bool
vchecklist xs = V.accum (||) (V.replicate (n+1) False) $ zip (filter ( <= n) xs) (repeat True)
    where n = length xs

-- O(n) using array
minfree' = search . checklist
-- O(n) using Vector
vminfree = vsearch . vchecklist

-- just another example for using array
countlist :: [Int] -> Int -> Array Int Int
countlist xs n = accumArray (+) 0 (0, n) $ zip xs (repeat 1)

countsort xs n = concat [ replicate k x | (x, k) <-  assocs . countlist xs $ n ]

-- divide & conquer: numbers in xs must be distinct
minfree'' xs = minfrom 0 (length xs, xs)
minfrom a (n, xs) | n == 0 = a
                  | m == b - a = minfrom b (n - m, vs)
                  | otherwise = minfrom a (m, us)
                  where (us, vs) = partition (< b) xs
                        b = a + 1 + n `div` 2
                        m = length us

-- tests
limit = 2^10
nats = listOf . choose $ (0, limit)
ok xs = all (== head xs) xs
prop = forAll nats $ \x -> ok (map ($x) [minfree, minfree', vminfree])

-- minfree'' assumes distinct input
prop2 = forAll (fmap nub nats) $ \x -> ok (map ($x) [minfree, minfree', minfree'', vminfree])

main = do
    print $ f [0..10]
    print $ f [2,4,6,8,5,0,3,5,1,2]
    quickCheck prop
    quickCheck prop2
    quickCheck (forAll nats $ \xs -> sort xs == countsort xs limit)
    where f xs = (minfree xs, minfree' xs)
