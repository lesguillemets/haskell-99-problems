import qualified Data.List

main = do
    print $ encode "aaaabccaadeeee"

pack :: (Eq a) => [a] -> [[a]]
pack = Data.List.group

encode :: (Eq a) => [a] -> [(Int,a)]
encode = map (\p->(length p, head p)) . pack
