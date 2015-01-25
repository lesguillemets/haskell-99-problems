import GHC.Exts (sortWith)
import Data.Function (on)
import Data.List (sort, groupBy)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))

original =  ["abc","de","fgh","de","ijkl","mn","o"]

lsort' :: [[a]] -> [[a]]
lsort' = sortWith length


lfsort'' :: [[a]] -> [[a]]
lfsort'' = concat . sortWith length . groupBy ((==) `on` length) . sortWith length

lfsort' :: [[a]] -> [[a]]
lfsort' li = sortWith ((v!) . length) li
    where
        v = freqTable li

-- freqTable :: [[a]] -> V.Vector Int
freqTable li = let ll = sort . map length $ li
                   lmax = 1 + last ll
                   in
                V.generate lmax (\n->length . takeWhile (==n) . dropWhile (/=n) $ ll)

main = do
    print $ lsort' original
    print $ lfsort' original
    print $ lfsort'' original
