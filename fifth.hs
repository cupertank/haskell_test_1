checker :: (a -> Bool) -> [a] -> Bool

checker f [] = True
checker f (x:xs) = if (f x) then checker f xs else False
