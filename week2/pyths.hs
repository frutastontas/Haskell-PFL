pyths :: Integer -> [(Integer, Integer, Integer)]
pyths n = [(i,j,k) | i <- [1..n], j<-[1..n], k<-[1..n], i*i + j*j == k*k ]