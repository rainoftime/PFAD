-- Selection Problem:
--
-- Given two *sorted* *disjoint* set X and Y
-- find the kth number of union(X,Y)

import Data.List
import Data.Array
import Control.Monad
import Test.QuickCheck
import Data.Ord

-- striaghtforward O(|X| + |Y|), or O(k)
smallest k (xs, ys) = union (xs, ys) !! k
    where union ([], ys) = ys
          union (xs, []) = xs
          union (xs'@(x:xs), ys'@(y:ys))
            | x < y = x : union (xs, ys')
            | y < x = y : union (xs', ys)

-- d&c on lists: still linear
smallest1 k ([], ws) = ws !! k
smallest1 k (zs, []) = zs !! k
smallest1 k (zs, ws) =
    case (a < b, k <= p + q) of
         (True, True) -> smallest1 k (zs, us)
         (True, False) -> smallest1 (k-p-1) (ys, ws)
         (False, True) -> smallest1 k (xs, ws)
         (False, False) -> smallest1 (k-q-1) (zs, vs)
    where p = length zs `div` 2
          q = length ws `div` 2
          (xs, a : ys) = splitAt p zs
          (us, b : vs) = splitAt q ws

-- d&c on array: O(log|X| + log|Y|)
smallest2 k (xa, ya) = search k (0, m+1) (0, n+1)
    where (0, m) = bounds xa
          (0, n) = bounds ya
          search k (lx, rx) (ly, ry)
            | lx == rx = ya ! (ly+k)
            | ly == ry = xa ! (lx+k)
            | otherwise = case (xa ! mx < ya ! my, k <= p + q) of
                             (True, True) -> search k (lx, rx) (ly, my)
                             (True, False) -> search (k-p-1) (mx+1,rx) (ly,ry)
                             (False,True) -> search k (lx, mx) (ly, ry)
                             (False,False) -> search (k-q-1) (lx,rx) (my+1,ry)
                          where mx = (lx + rx) `div` 2
                                my = (ly + ry) `div` 2
                                p = mx - lx
                                q = my - ly

-- dumb but obviously correct
dumb k (xs, ys) = sort (xs ++ ys) !! k

-- test
fromList xs = listArray (0, length xs - 1) xs

same xs = all (== head xs) xs

run xs ys k f = f k (xs, ys)

vals xs ys k = map (run xs ys k) [dumb, smallest, smallest1, smallest2']
    where smallest2' k (xs, ys) = smallest2 k (fromList xs, fromList ys)

evens n = [0,2..n]
odds n = [1,3..n]

ok1 n k = same $ k : vals (odds n) (evens n) k

prop1 = forAll (choose (1, 12345)) $ \n -> forAll (choose (0, n)) $ \k -> ok1 n k

arg :: Gen ([Integer], [Integer], Int)
arg = do
    zs <- liftM f $ oneof [listOf1 arbitrary, vector 6789]
    xs <- liftM f $ listOf (elements zs)
    let ys = zs \\ xs
    k <- choose (0, length zs - 1)
    return (xs, ys, k)
    where f = sort . nub

prop2 = forAll arg $ \(xs, ys, k) -> same $ vals xs ys k

maxlen = maximumBy (comparing f)
    where f (xs,ys,_) = length $ xs ++ ys

doit (xs, ys, k) = do
    putStrLn ""
    putStr "X " >> print xs
    putStr "Y " >> print ys
    putStr "k " >> print k
    putStr "result " >> print (vals xs ys k)
    putStrLn ""

main = do
    quickCheck prop1

    doit ([0..10], [11..20], 12)
    doit ([1,8,100,1000], [-3, 101, 102, 10000], 5)
    doit . maxlen =<< sample' arg

    quickCheck prop2
