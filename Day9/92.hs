main = do
  input <- readFile "input.txt"
  putStr "Uncompressed string length: "
  putStrLn . show . sumExpressions $ parseExpressions input

type Multiplier = Int
type SubSequence = String
type NestedExpressions = [CompExp]
data CompExp = TermExp Multiplier SubSequence | WrappingExp Multiplier NestedExpressions deriving (Show)

sumExpressions :: NestedExpressions -> Int
sumExpressions = foldr f 0
  where
    f (WrappingExp mult exps) acc = acc + mult * sumExpressions exps
    f (TermExp mult seq) acc = acc + mult * (length seq)

parseExpressions :: String -> NestedExpressions
parseExpressions [] = []
parseExpressions xs = if null prefix
                        then mainExp:parseExpressions unprocessed
                        else (TermExp 1 prefix
                             ):mainExp:parseExpressions unprocessed
  where
    mainExp = if '(' `elem` repSeq
                then WrappingExp (read multiplier) (parseExpressions repSeq)
                else TermExp (read multiplier) repSeq
    (prefix, rest1) = span (/= '(') xs
    (len, rest2) = if null rest1 then ("0",[]) else span (/= 'x') $ tail rest1
    (multiplier, rest3) = if null rest2 then ("0",[]) else span (/= ')') $ tail rest2
    (repSeq, unprocessed) = if null rest3 then ([],[]) else splitAt (read len) $ tail rest3
