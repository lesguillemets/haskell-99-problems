dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [ fst pair | pair <- zip xs [1..],
                              snd pair `mod` n /= 0]

dropEvery' :: [a] -> Int -> [a]
dropEvery' xs n = dropHelper' xs n 1 []
dropHelper' :: [a] -> Int -> Int -> [a] -> [a]
dropHelper' [] _ _ acc = reverse acc
dropHelper' (x:xs) n c acc
    | n == c    = dropHelper' xs n 1 acc
    | otherwise = dropHelper' xs n (c+1) (x:acc)

main = do
    print $ dropEvery "abcdefghijk" 3
    print $ dropEvery' "abcdefghijk" 3
