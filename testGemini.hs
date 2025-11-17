type Book = (String, Int)
type Library = [Book]

isQuickRead :: Book -> Bool
isQuickRead (name,pages) | pages < 50 = True
                         | otherwise = False



tearPages :: Book -> Int -> Book
tearPages (name,pages) teared = (name,pages-teared)


filterLongBooks :: Library -> (Book -> Bool) -> Library
filterLongBooks [] _ = []
filterLongBooks (b:books) pred | pred b = b:filterLongBooks books pred
                               | otherwise = filterLongBooks books pred

totalLibraryPages :: Library -> Int
totalLibraryPages lib = foldr (\(name,pages) acc -> pages+acc) 0 lib


extractSegment :: [a] -> Int -> Int -> [a]
extractSegment xs start end = [c | (c,index) <- zip xs [0..] , index >=start && index <= end]

isSubsequence :: String -> String -> Bool
isSubsequence [] (_:_) = False 
isSubsequence _ [] = True
isSubsequence (s:s1) (s2:sub) | s == s2 = isSubsequence s1 sub
                              | otherwise = isSubsequence s1 (s2:sub)

partitionByChar :: Library -> Char -> (Library, Library)
partitionByChar lib c = (filter (\(name,pages)-> isSubsequence name [c]) lib, filter (\(name,pages)-> not(isSubsequence name [c])) lib)

scrolls :: (Integral a) => [a]
scrolls = 1 : 2 : [ 3*y -x| (x,y)<-zip scrolls (tail scrolls)]