removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt n xs = removeAtHelper n xs []
removeAtHelper :: Int -> [a] -> [a] -> (Maybe a, [a])
removeAtHelper 0 (x:ls) rs = (Just x, ls++rs)
removeAtHelper n (x:ls) rs = removeAtHelper (n-1) ls (rs++[x])
removeAtHelper _ [] ls = (Nothing, ls)
-- won't handle n<0

main = do
    print $ removeAt 1 "abcd"
