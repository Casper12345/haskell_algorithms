module BubbleSort where

bubbleSort :: [a] -> (a -> a -> Bool) -> ([a], Bool)

bubbleSort xs f = helper xs [] f False

helper :: [a] -> [a] -> (a -> a -> Bool) -> Bool-> ([a], Bool)

helper [] acc f b = (acc, b)
helper (x : []) acc f b = (acc ++ [x], b)
helper (x : xs) acc f b = if(f x (head(xs))) then helper (x : tail(xs)) (acc ++ [head(xs)]) f True else helper xs (acc ++ [x]) f b