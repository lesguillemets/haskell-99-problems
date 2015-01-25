slice :: [a] -> Int -> Int -> [a]
slice xs n m = take (m+1-n) $ drop (n-1) xs

main = do
    print $ slice "abcdefghijk" 3 7
