main = do
    print $ isPalindrome "madamimadam"
    print $ isPalindrome' "madamimadam"

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome ls = ls == (reverse ls)

isPalindrome' ls = all (\(x,y) -> x==y) $ zip ls (reverse ls)
