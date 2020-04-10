import Control.Monad

printer n = do
	let weight = 1 + 2 * (n - 1)
	let printerHelper i = do
		let escapesNum = 1 - i + div weight 2 
		forM [1..escapesNum] $ \_ -> putStr " "
                forM [1.. weight - (escapesNum * 2)] $ \_ -> putStr "x"
                forM [1..escapesNum] $ \_ -> putStr " "
		putStrLn ""

	forM [1..n] $ \i -> printerHelper i
	forM (reverse [1..n-1]) $ \i -> printerHelper i
