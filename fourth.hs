supermap  :: [a] ->  (a -> [b]) -> [b]

supermap x f = concatMap f x
