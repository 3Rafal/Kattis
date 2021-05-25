open Scanf
open Printf

let main =
  scanf "%d" (fun n ->
    for i = 1 to n do
      printf "%d Abracadabra\n" i
    done)
