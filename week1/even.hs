evenalt :: Int -> String
evenalt xs = if xs `mod` 2 == 0
        then "Even"
        else "Odd"

