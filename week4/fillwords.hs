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


fillWords :: Int -> [AWord] -> Paragraph
fillWords _ [] = []
fillWords w ws =
  let (line, remain) = fitLine w ws
  in if null line
     then []                     
     else line : fillWords w remain



main :: IO ()
main = do  
    input <- getContents
    let output = unlines (map unwords (fillWords 70 (words input)))
    putStr output
    

