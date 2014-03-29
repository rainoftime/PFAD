import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Expr
import Text.Parsec.Language
import Data.Char
import Data.List
import Test.QuickCheck
import Control.Monad

-- this is my quick & dirty version
-- get all arrangments -> parsec -> filter
make :: [Integer] -> [String]
make [] = []
make [x] = [show x]
make (x:xs) = concat [ [s ++ y, s ++ " + " ++ y, s ++ "*" ++ y] | y <- make xs ]
    where s = show x

get :: String -> Integer
get s = case parse expr "" s of
             Right i -> i

solutions :: Integer -> [Integer] -> [String]
solutions ans = filter ((==ans) . get . strip) . make
    where strip = filter (/= ' ')

-- parser
lexer = makeTokenParser emptyDef
expr = buildExpressionParser table (decimal lexer)
table = [[op "*" (*) AssocLeft], [op "+" (+) AssocLeft]]
    where op s f = Infix (do {string s; return f})

-- print helper
doit sol ans digits = do
    putStrLn $ "get " ++ s ++ " from " ++ show digits ++ ":\n"
    mapM_ (putStrLn . f) $ sol ans digits
    putStrLn ""
    where s = show ans
          f x = s ++ " = " ++ x

-- this is actually like a top-down parsing
-- list is such a (universally) convenient data structure
type Expr = [Term]
type Term = [Factor]
type Factor = [Integer]

valExpr :: Expr -> Integer
valExpr = sum . map valTerm

valTerm :: Term -> Integer
valTerm = product . map valFact

valFact :: Factor -> Integer
valFact = foldl1 (\x d -> x * 10 + d)

showExpr :: Expr -> String
showExpr = intercalate " + " . map showTerm

showTerm :: Term -> String
showTerm = intercalate "*" . map showFact

showFact :: Factor -> String
showFact = map (intToDigit . fromIntegral)

good c = (== c)
solutions1 c = map showExpr . filter (good c . valExpr) . exprs

exprs :: [Integer] -> [Expr]
exprs = foldr extend []

extend :: Integer -> [Expr] -> [Expr]
extend x [] = [[[[x]]]]
extend x es = concatMap (glue x) es

glue :: Integer -> Expr -> [Expr]
glue x ((xs:xss):xsss) =
    [ ((x:xs) : xss) : xsss
    , ([x] : xs : xss) : xsss
    , [[x]] : (xs:xss) : xsss ]

-- fatest; shortest
-- foldr fusion works like a pruning dfs
good2 c (k,f,t,e) = f*t+e == c
ok2 c (k,f,t,e) = f*t+e <= c
solutions2 c = map (showExpr . fst) . filter (good2 c . snd) . foldr (expand c) [([],(0,0,0,0))]
expand c x [] = []
expand c x [([],_)] = [([[[x]]], (10,x,1,0))]
expand c x es = concatMap (filter (ok2 c . snd) . glue2 x) es
glue2 x ((xs:xss):xsss, (k,f,t,e)) =
    [ (((x:xs) : xss) : xsss, (10*k, k*x + f, t, e))
    , (([x] : xs : xss) : xsss, (10, x, f*t, e))
    , ([[x]] : (xs:xss) : xsss, (10, x, 1, f*t + e))]

-- tests
digits = liftM (take 9) . listOf $ elements [1..9]
dst = choose (1,1000)
same xs = all (== head xs) xs
run c ds = map (\s -> sort $ s c ds) [solutions, solutions1, solutions2]
prop = forAll (liftM2 (,) digits dst) $ \(ds, c) -> same $ run c ds

-- some examples
main = do
    doit solutions 100 [1..9]
    doit solutions1 1234 $ map (fromIntegral . digitToInt) $ take 12 . drop 2 $ show pi
    doit solutions2 9876 $ map (fromIntegral . digitToInt) $ take 20 . drop 2 $ show (exp 1)
    verboseCheck prop
