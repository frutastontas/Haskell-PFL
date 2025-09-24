safetail :: [a]-> [a]
safetail xs = case xs of 
            [] -> []
            _ -> tail xs