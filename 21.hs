insertAt :: a -> [a] -> Int -> [a]
insertAt el xs n = take n xs ++ el:(drop n xs)

main = do
    print $ insertAt 'X' "abcd" 2
