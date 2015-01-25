split :: [a] -> Int -> ([a],[a])
split xs n = (take n xs, drop n xs)

split' = splitHelper' []
splitHelper' :: [a] -> [a] -> Int -> ([a],[a])
splitHelper' bfr aft 0 = (reverse bfr,aft)
splitHelper' bfr (x:xs) n = splitHelper' (x:bfr) xs (n-1)
splitHelper' _ [] _ = error "index out of range"

main = do
    print $ split "abcdefghik" 3
    print $ split' "abcdefghik" 3
