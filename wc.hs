
count_chars :: [String] -> Int
count_chars ws = length (unwords ws)

count_words :: [String] -> Int
count_words [] = 0
count_words (w:ws) = 1 + count_words ws



wc :: [[String]] -> (Int,Int,Int)
wc para = loop para 0 0 0
    where
        loop [] nlines nwords nchars = (nlines, nwords, nchars) 
        loop (w:ws) nlines nwords nchars  = loop ws (nlines+1) (nwords + count_words w) (nchars + count_chars w)




main :: IO ()
main = do 
    putStrLn "Enter your file"
    inp <- getContents
    let (lineCount, wordCount, charCount) = wc (map words (lines inp))
    
    putStrLn $ "Lines: " ++ show lineCount
    putStrLn $ "Words: " ++ show wordCount
    putStrLn $ "Characters: " ++ show charCount



