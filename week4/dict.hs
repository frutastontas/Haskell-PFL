
import GHC.Arr (fill)
type Dict = [String]

readDict :: IO Dict
readDict = do 
    txt <- readFile "/usr/share/dict/words"
    return (words txt)


checkWord :: Dict-> String-> String
checkWord dic s = if (elem s dic) then s else "\ESC[7m"++s++"\ESC[0m"


spellaux :: Dict-> [[String]] -> [[String]]
spellaux [] para = para
spellaux _ [] = []
spellaux dic (xs:xss) = (map (checkWord dic) xs ) : spellaux dic xss



spellauxalt :: Dict -> String -> String
spellauxalt dic str = unwords processed
                where processed = map (checkWord dic) (words str)
                    
spellCheckalt :: Dict-> String-> String 
spellCheckalt dic s = unlines text
                where 
                    text = map (spellauxalt dic) lined
                    lined = lines s

spellCheck :: Dict-> String-> String 
spellCheck dic s = unlines (map unwords (spellaux dic (map words (lines s))))

main :: IO ()
main = do 
    dict <- readDict
    print (length dict)