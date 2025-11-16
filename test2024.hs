type Match = ((String,String), (Int,Int))
type MatchDay = [Match]
type League = [MatchDay]

myLeague :: League
myLeague = [
    [(("Porto","Sporting"),(2,2)),(("Benfica","Vitoria SC"),(4,0))],
    [(("Porto","Benfica"),(5,0)),(("Vitoria SC","Sporting"),(3,2))],
    [(("Vitoria SC","Porto"),(1,2)),(("Sporting","Benfica"),(2,1))]
    ]

winner :: Match -> String
winner ((equipa1,equipa2),(golos1,golos2)) | golos1 == golos2 = "draw"
                                           | golos1 > golos2 = equipa1
                                           | otherwise = equipa2


matchDayScore :: String -> MatchDay -> Int
matchDayScore _ [] = 0
matchDayScore team (((equipa1,equipa2),(golos1,golos2)):xs) | team == equipa1 || team == equipa2 = if (winner match == team) then 3 else if(winner match == "draw") then 1 else 0
                                                            | otherwise = matchDayScore team xs
                                                            where match = ((equipa1,equipa2),(golos1,golos2))                                      


-- functions to use ------------------------------
leagueScore :: String -> League -> Int
leagueScore t = foldr (\d acc -> matchDayScore t d + acc) 0


sortByCond :: Ord a => [a] -> (a -> a -> Bool) -> [a]
sortByCond [] _ = []
sortByCond [x] _ = [x]
sortByCond l cmp = merge (sortByCond l1 cmp) (sortByCond l2 cmp) cmp
    where (l1 ,l2) = splitAt (div (length l) 2) l


merge :: Ord a => [a] -> [a] -> (a -> a -> Bool) -> [a]
merge [] l _ = l
merge l [] _ = l
merge (x:xs) (y:ys) cmp
    | cmp x y = x:(merge xs (y:ys) cmp)
    | otherwise = y:(merge (x:xs) ys cmp)

---------------------------------------------------------------------

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

getTeams :: League -> [String]
getTeams league = nub ([ home | mday <-league,((home,away),_)<-mday] ++ [away | mday <- league,((home,away),_)<-mday])

ranking:: League -> [(String,Int)]
ranking league = sortByCond [(team, leagueScore team league) | team <-(getTeams league)] compareFunction


compareFunction :: (String,Int) -> (String,Int) -> Bool
compareFunction (team1,points1) (team2, points2) | points1 /= points2 = points1 > points2
                                                 | otherwise = team1 < team2

matchDayWithDraw :: MatchDay -> Bool
matchDayWithDraw mday = any (\match -> winner match == "draw") mday

numMatchDaysWithDraws :: League -> Int
numMatchDaysWithDraws league = foldr (\mday acc -> if (matchDayWithDraw mday) then 1+ acc else acc) 0 league


bigWins :: League -> [(Int,[String])]
bigWins league = [(index,[ winner ((t1,t2),(g1,g2)) | ((t1,t2),(g1,g2)) <- mday, abs(g1-g2) >= 3 ]) | (mday, index)<- zip league [1..]]

type City = String
type Path = [City]
type Distance = Int
type RoadMap = [(City,City,Distance)]

gTest1 :: RoadMap
gTest1 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]
gTest2 :: RoadMap -- unconnected graph
gTest2 = [("0","1",4),("2","3",2)]



adjacent :: RoadMap -> City ->[(City,Distance)]
adjacent [] _ = []
adjacent ((src,dest,dist):xs) city | city == src = (dest,dist) : adjacent xs city
                                   | otherwise = adjacent xs city




data KdTree = Empty | Node Char (Int,Int) KdTree KdTree deriving (Eq,Show)
tree1 :: KdTree
tree1 = Node 'x' (3,3) (Node 'y' (2,2) Empty Empty) (Node 'y' (4,4) Empty Empty)
tree2 :: KdTree
tree2 = Node 'x' (3,3) (Node 'y' (2,2) (Node 'x' (1,1) Empty Empty) Empty) (Node 'y' (4,4) (Node 'x' (3,2) Empty Empty) Empty)

insert :: (Int,Int) -> KdTree -> KdTree
insert (x,y) tree = insertAux (x,y) 'x' tree


insertAux :: (Int,Int) -> Char -> KdTree -> KdTree
insertAux (x,y) tipo Empty = Node tipo (x,y) Empty Empty
insertAux (x,y) tipo (Node t (xn,yn) left right)    | x==xn && y ==yn = (Node t (xn,yn) left right) -- if already in tree
                                                    | otherwise = case t of
                                                            'x' -> if (x>=xn) then Node t (xn,yn) left (insertAux (x,y) 'y' right) 
                                                            else Node t (xn,yn) (insertAux (x,y) 'y' left) right

                                                            'y' -> if (y>=yn) then Node t (xn,yn) left (insertAux (x,y) 'x' right) 
                                                            else Node t (xn,yn) (insertAux (x,y) 'x' left) right
