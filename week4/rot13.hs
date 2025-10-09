import Data.Char

rot13char :: Char -> Char
rot13char c =
  if isLetter c
    then if isUpper c
           then chr (((ord c - ord 'A' + 13) `mod` 26) + ord 'A')
           else chr (((ord c - ord 'a' + 13) `mod` 26) + ord 'a')
    else c
                            -- use mod 26 to wrap around after addition

rot13 :: String -> String
rot13 s = map rot13char s



main :: IO ()
main = do 
    putStrLn "You may enter a string to encode"
    inp <- getLine
    putStrLn (rot13 inp)