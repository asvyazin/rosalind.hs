square :: Float -> Float
square x = x * x

prob1 :: Float -> Float
prob1 x = 0.5 * (square x + square (1 - x))

prob = map prob1

readFloat :: String -> Float
readFloat = read

parseInput :: String -> [Float]
parseInput str = map readFloat $ words str

answer :: String -> String
answer = unwords . map show . prob . parseInput