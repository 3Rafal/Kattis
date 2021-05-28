(*
5
3 4 1 7 2
 *)

open Scanf
open Printf

let main =
      scanf "%d\n" (fun k ->
        let min = ref 1000000000 in
        let ind = ref 0 in
        for i = 0 to (k-1) do 
          scanf " %d"
            (fun v ->
              if v < !min then (min := v; ind := i)
              ) 
        done;
        scanf "\n" ();
        printf "%d\n" !ind
      )
