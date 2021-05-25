open Scanf
open Printf

let rec go n i cap result =
  if i = n then cap + result
  else
    scanf "%d\n" (fun m ->
        go n (i+1) cap (result + cap - m))

let main =
  scanf "%d\n" (fun cap ->
      scanf "%d\n" (fun n ->
          printf "%d\n" (go n 0 cap 0)))
