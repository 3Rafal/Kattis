-- https://open.kattis.com/problems/greetings2
countEsTwice :: String -> Int
countEsTwice = (* 2) . length . takeWhile (/= 'y') . tail

run :: String -> String
run s  = "h" ++ replicate (countEsTwice s) 'e' ++ "y\n" 

main :: IO ()
main = interact run
