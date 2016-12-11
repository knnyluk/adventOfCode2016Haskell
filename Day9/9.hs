
main = do
  input <- readFile "input.txt"
  let exps = stringToExpressions input

  putStr . show $ countChars exps

countChars :: [Expression] -> Int
countChars xs = foldr f 0 xs
  where
    f x acc = acc + expLength x

expLength :: Expression -> Int
expLength (prefix, multiplier, repSeq, _) = (length prefix) + (length repSeq) * multiplier

flattenExpression :: Expression -> String
flattenExpression (prefix, multiplier, repSeq, _) = concat $ prefix:replicate multiplier repSeq

stringToExpressions :: String -> [Expression]
stringToExpressions [] = []
stringToExpressions xs = exp:stringToExpressions rest
  where
    exp@(_,_,_,rest) = parseExpression xs

type Expression = (String, Int, String, String)

parseExpression :: String -> Expression
parseExpression xs = (prefix, read multiplier, repSeq, rest4)
  where
    (prefix, rest1) = span (/= '(') xs
    (len, rest2) = if null rest1 then ("0",[]) else span (/= 'x') $ tail rest1
    (multiplier, rest3) = if null rest2 then ("0",[]) else span (/= ')') $ tail rest2
    (repSeq, rest4) = if null rest3 then ([],[]) else splitAt (read len) $ tail rest3
