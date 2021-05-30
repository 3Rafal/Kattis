-- https://open.kattis.com/problems/timeloop

main = interact $ init . unlines . fmap ((++ " Abracadabra"). show) . enumFromTo 1 . read
