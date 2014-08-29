main = do
    print $ myLength "here is"
    print $ myLength' "here is"
    print $ myLength'' "here is"

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLengthac [] n = n
myLengthac (_:xs) n = myLengthac xs (n+1)
myLength' ls = myLengthac ls 0

myLength'' = foldr (const (+1)) 0
