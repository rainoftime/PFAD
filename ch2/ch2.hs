import Data.List
import Test.QuickCheck

-- straightforward O(n^2)
msc :: Ord a => [a] -> Int
msc xs = maximum [scount z zs | z : zs <- tails xs]

scount x xs = length (filter (x <) xs)

-- d&c O(nlogn)
msc' :: Ord a => [a] -> Int
msc' = maximum . map snd . table

table [x] = [(x, 0)]
table xs = join (m-n) (table ys) (table zs)
    where m = length xs
          n = m `div` 2
          (ys, zs) = splitAt n xs

join _ txs [] = txs
join _ [] tys = tys
join n txs@((x,c):txs') tys@((y,d):tys')
    | x < y = (x, c+n) : join n txs' tys
    | otherwise = (y,d): join (n-1) txs tys'

ints :: Gen [Int]
ints = listOf arbitrary

nstr :: Gen [String]
nstr = listOf1 arbitrary

main = do
    print (msc as, msc' as)
    print (msc bs, msc' bs)
    print (msc cs, msc' cs)
    print (msc ds, msc' ds)

    quickCheck . forAll ints $ \xs -> (not . null) xs ==> msc xs == msc' xs
    quickCheck . forAll nstr $ \xs -> msc xs == msc' xs

    where as = "GENERATING"
          bs = [1,3,4,6,2,0,8,9,3,6,5,1,100,99,0,3,7,1,2]
          cs = [1..1000]
          ds = [1000,999..1]
