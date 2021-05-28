open Scanf
open Printf

let process x y n =
  if n mod x = 0 && n mod y = 0 then "FizzBuzz"
  else if n mod x = 0 then "Fizz"
  else if n mod y = 0 then "Buzz"
  else sprintf "%d" n
  
let main =
  scanf "%d %d %d\n" (fun x y n ->
      for i = 1 to n do
        printf "%s\n" (process x y i)
      done)
