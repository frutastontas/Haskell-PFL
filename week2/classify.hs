classify :: Int-> String
classify xs = if xs <= 20 && xs > 18 then "excellent"
              else if xs <= 18 && xs >15 then "very good"
              else if xs <= 15 && xs > 12 then "good"
              else if xs <= 12 && xs > 9 then "passed"
              else "failed"
-- this definition is using conditionals and not guards

classifyalt :: Int -> String
classifyalt xs
    | xs <= 20 && xs > 18 = "excellent"
    | xs <= 18 && xs > 15 = "very good"
    | xs <= 15 && xs > 12 = "good"
    | xs <= 12 && xs > 9 = "passed"
    | otherwise           = "failed"

