-- https://open.kattis.com/problems/sodaslurper
  
exchangeAndDrink :: Int -> (Int, Int) -> (Int, Int)
exchangeAndDrink cost (empty, drinked) =
  if empty < cost then (empty, drinked)
  else exchangeAndDrink cost (remaining, drinked + exchanged)
    where exchanged = empty `quot` cost
          remaining = empty `rem` cost + exchanged
          
solve inp =
  exchangeAndDrink cost (empty, 0)
  where ws = words inp
        cost = read $ ws !! 2
        empty = read (ws !! 0) + read (ws !! 1) 

run = show . snd . solve
main = interact run 

inp1 = "9 0 3"
inp2 = "5 5 2"
