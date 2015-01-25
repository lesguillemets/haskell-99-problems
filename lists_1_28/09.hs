import qualified Data.List

main = do
    print $ pack "aaaabccaadeeee"
    print $ pack' "aaaabccaadeeee" []
    print $ pack'' "aaaabccaadeeee" 

pack :: (Eq a) => [a] -> [[a]]
pack = Data.List.group

pack' [] packed = reverse packed
pack' (x:xs) (p:pc)
    | x == head p = pack' xs ((x:p):pc)
    | otherwise   = pack' xs ([x]:p:pc)
pack' (x:xs) _ = pack' xs [[x]]

pack'' [] = []
pack'' (x:xs) = (x:takeWhile (==x) xs ) : (pack'' (dropWhile (==x) xs))
