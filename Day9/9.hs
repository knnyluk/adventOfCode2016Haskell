
main = do
  input <- readFile "input.txt"
  let exps = stringToExpressions input

  putStr . show $ countChars exps

countChars :: [Expression] -> Int
countChars xs = foldr f 0 xs
  where
    f x acc = acc + expLength x

expLength :: Expression -> Int
expLength (EndingExpression prefix rest) = (length prefix) + length rest
expLength (NestedExpression prefix multiplier nestedExp rest) = (length prefix) + (expLength nestedExp) * multiplier

--flattenExpression :: Expression -> String
--flattenExpression (prefix, multiplier, repSeq, _) = concat $ prefix:replicate multiplier repSeq

stringToExpressions :: String -> [Expression]
stringToExpressions [] = []
stringToExpressions xs = exp:(stringToExpressions $ getRest exp)
  where
    exp = parseExpression xs

getRest :: Expression -> String
getRest (EndingExpression _ rest) = rest
getRest (NestedExpression _ _ _ rest) = rest

data Expression = EndingExpression String String| NestedExpression String Int Expression String deriving (Show)

parseExpression :: String -> Expression
parseExpression xs = if multiplier /= "0"
                       then NestedExpression prefix (read multiplier) (parseExpression repSeq) rest4
                       else EndingExpression prefix rest4
  where
    (prefix, rest1) = span (/= '(') xs
    (len, rest2) = if null rest1 then ("0",[]) else span (/= 'x') $ tail rest1
    (multiplier, rest3) = if null rest2 then ("0",[]) else span (/= ')') $ tail rest2
    (repSeq, rest4) = if null rest3 then ([],[]) else splitAt (read len) $ tail rest3
