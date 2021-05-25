open Scanf
open Printf

let rec go n i result =
  if i = n then result
  else
    scanf "%f %f\n" (fun q ys ->
        go n (i+1) (result +. (q *. ys)))

let main =
  scanf "%d\n" (fun n ->
      printf "%f\n" (go n 0 0.0))

