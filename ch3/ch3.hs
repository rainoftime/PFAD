-- assuming f (x,y) is O(1) (though sometimes it's not -- maybe expensive),
-- we mainly analyze the complexity of counts of the f(x,y) evaluations
--
-- note that, we take `f' as unknown black box. if you know and can inspect
-- `f' in advance, you can use some specific optimizing techniques
--
-- in ghci, you can :set +s to see the running time of different versions
--

import Data.List (sort)
import Test.QuickCheck
import Control.Monad (liftM, liftM3)
import Data.Ord (comparing)

-- as always, the first is no-brainer brute. O(z^2)
invert0 f z = [(x,y) | x <- [0..z], y <- [0..z], f (x,y) == z]

-- elegant saddleback O(z)
invert1 f z = find (0,z) f z z
find (u,v) f z n
    | u > n || v < 0 = []
    | z' < z = find (u+1, v) f z n
    | z' > z = find (u, v-1) f z n
    | otherwise = (u,v) : find (u+1, v-1) f z n
    where z' = f (u,v)

-- like invert1, but first get domain range.  O(logz + m + n)
invert2 f z = find (0,m) f z n
    where m = bsearch (\y -> f (0,y)) (-1,z+1) z
          n = bsearch (\x -> f (x,0)) (-1,z+1) z

-- biggest x in [a,b) s.t. g x <= z < g (x+1)
bsearch g (a,b) z
    | a+1 == b = a
    | g m <= z = bsearch g (m,b) z
    | otherwise = bsearch g (a,m) z
    where m = (a+b) `div` 2

-- like invert2, but just enumerate to get the range. O(m+n)
invert2' f z = find (0,m) f z n
    where m = head $ dropWhile (\y -> f(0, y) < z) [0..]
          n = head $ dropWhile (\x -> f(x, 0) < z) [0..]

-- divide & conquer take 1: split into three rectangles.
-- O(logz + m^1.59*log(2n/m)) m = min(m,n), n = max(m, n)
invert3 f z = find3 (0,m) (n,0) f z
    where m = bsearch (\y -> f (0,y)) (-1,z+1) z
          n = bsearch (\x -> f (x,0)) (-1,z+1) z

find3 (u,v) (r,s) f z
    | u > r || v < s = []
    | z' < z = find (u,v) (p,q+1) ++ find (p+1,q-1) (r,s) ++ find (p+1,v) (r,q)
    | z' > z = find (u,v) (p-1,q) ++ find (p+1,q-1) (r,s) ++ find (u,q-1) (p,s)
    | otherwise = (p, q) : find (p+1,q-1) (r,s) ++ find (u,v) (p-1,q+1)
    where p = (u + r) `div` 2
          q = (v + s) `div` 2
          z' = f (p, q)
          find a b = find3 a b f z

-- like invert3, but just enumerate to get the range.
-- O(m + n + m^1.59*log(2n/m)) m = min(m,n), n = max(m, n)
invert3' f z = find3 (0,m) (n,0) f z
    where m = head $ dropWhile (\y -> f(0, y) < z) [0..]
          n = head $ dropWhile (\x -> f(x, 0) < z) [0..]

-- faster divide & conquer: binary search on a *fixed* row/col
-- split into two-rectangles
-- O(logz + mlog(n/m)) m = min(m,n), n = max(m, n)
invert4 f z = find4 (0,m) (n,0) f z
    where m = bsearch (\y -> f (0,y)) (-1,z+1) z
          n = bsearch (\x -> f (x,0)) (-1,z+1) z

find4 (u,v) (r,s) f z
    | u > r || v < s = []
    | v - s <= r - u = rfind (bsearch (\x -> f(x,q)) (u-1,r+1) z)
    | otherwise = cfind (bsearch (\y -> f(p,y)) (s-1,v+1) z)
    where p = (u + r) `div` 2
          q = (v + s) `div` 2
          rfind p = (if f (p,q) == z then (p,q):find4 (u,v) (p-1,q+1) f z
                    else find4 (u,v) (p, q+1) f z) ++
                    find4 (p+1, q-1) (r,s) f z
          cfind q = find4 (u,v) (p-1,q+1) f z ++
                    (if f (p,q) == z then (p,q):find4 (p+1,q-1) (r,s) f z
                    else find4 (p+1, q) (r,s) f z)


-- tests
nat = choose (0::Integer, 100)

f0 (x,y) = 2^y * (2*x+1) - 1
f1 (x,y) = x*2^x + y*2^y + 2*x + y
f2 (x,y) = 3*x + 37*y + y^2
f3 (x,y) = x^2 + y^2 + x + y
f4 (x,y) = x + 2^y + y - 1

same xs = all (== head xs) xs

invs = [invert0, invert1, invert2, invert2', invert3, invert3', invert4]
fs = [f0, f1, f2, f3, f4]

results z f = map (r z f) invs
    where r z f i = sort $ i f z

prop f = forAll nat $ \z -> same $ results z f
check = quickCheck . prop
checkall = quickCheck . conjoin $ map prop fs

-- Lets generate random functions
data Expr = Bin Op Expr Expr
          | Const Integer
          | Var String deriving (Eq)

data Op = Add | Mul | Pow deriving (Eq, Enum)

instance Show Op where
    show Add = "+"
    show Mul = "*"
    show Pow = "^"

instance Show Expr where
    show (Const x) = show x
    show (Var v) = v
    show (Bin o e1 e2) = "(" ++ show e1 ++ show o ++ show e2 ++ ")"

instance Arbitrary Op where
    arbitrary = frequency [(64, return Add), (32, return Mul), (1, return Pow)]

instance Arbitrary Expr where
    arbitrary =
        oneof [ liftM Const (elements [0..10])
              , liftM Var (elements ["x", "y"])
              , liftM3 Bin arbitrary arbitrary arbitrary
              ]

eval (Const x) _ = x
eval (Var "x") (x,_) = x
eval (Var "y") (_,y) = y
eval (Bin o e1 e2) p = f o (eval e1 p) (eval e2 p)
    where f Add = (+)
          f Mul = (*)
          f Pow = (^)

-- f(x,y) must be strictly increasing
xplusy = Bin Add (Var "x") (Var "y")
func = liftM (Bin Add xplusy) arbitrary
prop' = forAll func $ \f -> prop (eval f)

instance Ord Expr where
    compare = comparing (length . show)

main = do
    check f0
    check f3
    verboseCheck . prop $ f2
    checkall

    f <- liftM maximum $ sample' func
    putStrLn $ "find " ++ show f ++ " == 123:"
    mapM_ print $ results 123 (eval f)

    verboseCheck prop'
