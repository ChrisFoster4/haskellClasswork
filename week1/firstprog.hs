module Main where

mult2 :: Int -> Int -- Doubles input val
mult2 x = x * 2

multi4 :: Int -> Int
multi4 x = mult2 $ mult2 x

main = do
	putStrLn "mult2 5 called"
	print $ mult2 5
	putStrLn "mult 4 5 called"
	print $ multi4 5
