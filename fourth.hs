groupElems [] = []
groupElems (x:xs) = reverse (lastGroup:ans)
    where
        (ans, lastGroup, _) = foldl groupElems' ([], [x], x) xs

groupElems' (ans, group, elem) x
                                | x == elem = (ans, (x:group), elem)
                                | x /= elem = ((group:ans), [x], x) 