inputFileName = "input.txt"
noMultiplier = 1

main = do
  input <- readFile inputFileName
  putStr $ "Uncompressed string length for file: " ++ inputFileName ++ " is "
  putStrLn . show . calculateExpressionLength $ parseExpressions input

type Multiplier           = Int
data CompressedExpression = TerminalExp Multiplier String | WrappingExp Multiplier NestedExpressions deriving (Show)
type NestedExpressions    = [CompressedExpression]

buildCompressedExpression :: Multiplier -> String -> CompressedExpression
buildCompressedExpression multiplier repSeq = if '(' `elem` repSeq
                                                then WrappingExp multiplier $ parseExpressions repSeq
                                                else TerminalExp multiplier repSeq

parseExpressions :: String -> NestedExpressions
parseExpressions [] = []
parseExpressions xs = if null prefix
                        then mainExp:parseExpressions remainingUnprocessedStr
                        else leadingExp:mainExp:parseExpressions remainingUnprocessedStr
  where
    leadingExp                            = buildCompressedExpression noMultiplier prefix
    mainExp                               = buildCompressedExpression (read multiplier) repSeq
    (prefix, rst1)                        = span (/= '(') xs
    (repSeqLength, rst2)                  = splitOnAndExclude 'x' rst1 ("0",[])
    (multiplier, rst3)                    = splitOnAndExclude ')' rst2 ("0",[])
    (repSeq, remainingUnprocessedStr)     = if null rst3 then ([],[]) else splitAt (read repSeqLength) $ tail rst3
    splitOnAndExclude _ [] fallbackReturn = fallbackReturn
    splitOnAndExclude charToSplitOn str _ = span (/= charToSplitOn) $ tail str

calculateExpressionLength :: NestedExpressions -> Int
calculateExpressionLength = foldr f 0
  where
    f (WrappingExp mult exps) acc = acc + mult * calculateExpressionLength exps
    f (TerminalExp mult seq) acc  = acc + mult * length seq
