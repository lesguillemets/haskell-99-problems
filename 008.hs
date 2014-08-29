import qualified Data.List

main = do
    print $ compress "aaaabccaadeeee"
    print $ compress' "aaaabccaadeeee"
    print $ compress'' "aaaabccaadeeee"
    print $ compress''' "aaaabccaadeeee"

compress :: (Eq a) => [a] -> [a]
compress (x0:x1:xs)
    | x0 == x1  = compress (x1:xs)
    | otherwise = x0:compress (x1:xs)
compress (x:[]) = [x]
compress [] = []

compress' xs = compressAcc xs []
compressAcc (x0:x1:xs) acc
    | x0 == x1  = compressAcc (x1:xs) acc
    | otherwise = compressAcc (x1:xs) (x0:acc)
compressAcc (x:[]) acc = compressAcc [] (x:acc)
compressAcc [] acc = reverse acc

compress'' [] = []
compress'' (x:xs) = x : (compress'' $ dropWhile (==x) xs)

compress''' :: (Eq a) => [a] -> [a]
compress''' = (map head) . Data.List.group
