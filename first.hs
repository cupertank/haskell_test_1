a = map ((-1) ^) [0, 1 ..]
b = map (\x -> fst x * snd x)  (zip a [1, 2 ..])
