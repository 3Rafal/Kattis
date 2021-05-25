open Scanf
open Printf

(*
mean = (r1+r2)/2, so r2 = (2 * mean) - r1
 *)
let main =
  scanf "%d " (fun r1 ->
      scanf "%d" (fun mean ->
          printf "%d" ((2 * mean) - r1)))
