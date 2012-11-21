import Text.ParserCombinators.Parsec

percent :: Int -> Int -> Float
percent n full =
  (nF / fullF) * 100
  where
    nF = fromIntegral n
    fullF = fromIntegral full
  
gc :: String -> Float
gc str =
  percent ngc len
  where
    len = length str
    ngc = length $ filter isGC str
    isGC 'G' = True
    isGC 'C' = True
    isGC _ = False
    
type Entry = (String, String)
type Dataset = [Entry]

dataset :: CharParser () Dataset
dataset = many entry

entry :: CharParser () Entry
entry = do
  char '>'
  k <- key
  char '\n'
  v <- value
  return (k, v)
  
key :: CharParser () String
key = many1 (letter <|> digit <|> char '_')

value :: CharParser () String
value = many1 (letter <|> char '\n') >>= return . glue

glue :: String -> String
glue str = filter (/= '\n') str