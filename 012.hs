import qualified Data.List

main = do
    print $ decode $ encodeModified "aaaabccaadeeee"

data Encoded a = Single a | Multiple(Int,a) deriving (Show)

decode :: [Encoded a] -> [a]
decode = concatMap dec

dec :: Encoded a -> [a]
dec (Single x) = [x]
dec (Multiple (n,s)) = [ s | _ <- [1..n] ]

pack :: (Eq a) => [a] -> [[a]]
pack = Data.List.group

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified = map enc . pack

enc :: (Eq a) => [a] -> Encoded a
enc (x:[]) = Single x
enc xs = Multiple (length xs,head xs)
