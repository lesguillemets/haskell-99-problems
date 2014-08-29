repli :: [a] -> Int -> [a]
repli ls n = concatMap (replicate n) ls

main = do
    print $ repli "abc" 3
