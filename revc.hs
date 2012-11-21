revc str = reverse $ map complement str
  where
    complement 'A' = 'T'
    complement 'T' = 'A'
    complement 'G' = 'C'
    complement 'C' = 'G'