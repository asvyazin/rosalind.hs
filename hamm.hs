hamm [c] [d]
  | c == d = 0
  | otherwise = 1
hamm (c:cs) (d:ds)
  | c == d = h
  | otherwise = h + 1
  where h = hamm cs ds