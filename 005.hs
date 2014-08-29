main = do
    print $ myReverse "A man, a plan, a canal, panama!"
    print $ myReverse' "A man, a plan, a canal, panama!"
    print $ myReverse'' "A man, a plan, a canal, panama!"

myReverse :: [a] -> [a]
myReverse = reverse

myReverse' xs = myReverseac xs []
myReverseac [] acc = acc
myReverseac (x:xs) acc = myReverseac xs (x:acc)

-- slow, of course
myReverse'' [] = []
myReverse'' (x:xs) = myReverse' xs ++ [x]

