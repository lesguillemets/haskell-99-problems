main = do
    print $ rotate "abcdefgh" 3
    print $ rotate "abcdefgh" (-2)
    print $ rotate' "abcdefgh" 3
    print $ rotate' "abcdefgh" (-2)

rotate :: [a] -> Int -> [a]
rotate xs n = (drop m xs) ++ (take m xs)
    where
        m = n `mod` (length xs)

rotate' xs n = take len . drop m . cycle $ xs
    where
        len = length xs
        m = n `mod` len
