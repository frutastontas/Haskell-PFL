checkTriangle :: Float-> Float-> Float-> Bool
checkTriangle a b c = x1 > c && x2 > b && x3 > a
                        where x1 = a+b
                              x2 = a+c
                              x3 = b+c

