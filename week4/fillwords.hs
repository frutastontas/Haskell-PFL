import GHC.Arr (fill)
type AWord = String
type Line= [AWord]
type Paragraph = [Line]




fillWords :: Int -> [AWord] -> Paragraph
fillWords _ [] = []
fillWords maxWidth words = loop words [] 0 []
  where
    loop [] currentLine _ acc = reverse (reverse currentLine : acc) -- base case: if we have no words left to take we put our current line in the acc and then return it
    loop (w:ws) currentLine currentLength acc
      | currentLength == 0 = loop ws (w:currentLine) (length w) acc
      | (currentLength + 1 + length w) <= maxWidth = loop ws (w:currentLine) (currentLength + 1 + length w) acc -- if word fits then put it in the current line
      | otherwise = loop (w:ws) [] 0 (reverse currentLine : acc)  -- if word does not fit then we reset out currentLine and Length and add out line we just built into our acc




main :: IO ()
main = do  
    input <- getLine
    let output = unlines (map unwords (fillWords 70 (words input)))
    putStr "This is how your line appears as a paragraph"
    putStr output
    

