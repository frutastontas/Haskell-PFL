short :: [a]-> Bool
short [] = True
short [_] = True
short [_,_] = True
short (_:_:_:_) = False -- three or more elements




