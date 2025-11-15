
count_chars :: String -> Int
count_chars str = length str


count_words :: String -> Int
count_words str = length (words str)


wc :: [String] -> (Int,Int,Int)
wc para = loop para 0 0 0
    where
        loop [] nlines nwords nchars = (nlines, nwords, nchars) 
        loop (w:ws) nlines nwords nchars  = loop ws (nlines+1) (nwords + count_words w) (nchars + count_chars w +1)




main :: IO ()
main = do 
    inp <- getContents
    let (lineCount, wordCount, charCount) = wc (lines inp)
    
    putStrLn $ show lineCount
    putStrLn $ show wordCount
    putStrLn $  show charCount



