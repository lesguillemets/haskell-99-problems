import qualified Data.List

main = do
    print $ encodeModified "aaaabccaadeeee"

data Encoded a = Single a | Multiple(Int,a) deriving (Show)

pack :: (Eq a) => [a] -> [[a]]
pack = Data.List.group

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified = map enc . pack

enc :: (Eq a) => [a] -> Encoded a
enc (x:[]) = Single x
enc xs = Multiple (length xs,head xs)
