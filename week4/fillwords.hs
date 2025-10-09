import GHC.Arr (fill)
type AWord = String
type Line= [AWord]
type Paragraph = [Line]

fitLine :: Int -> [AWord] -> ([AWord], [AWord])

fitLine width xs = loop xs 0 []
  where
    loop [] _ acc = (reverse acc, [])
    loop (w:ws) currWidth acc
      | currWidth + length w <= width
          = loop ws (currWidth + length w) (w : acc)
      | otherwise
          = (reverse acc, w:ws)


fillWords :: Int-> [AWord]-> Paragraph
fillWords _ [] = []
fillWords w words = line : fillWords w remain
                where (line, remain) = fitLine w words




