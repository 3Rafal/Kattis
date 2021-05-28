(*
2 4 10
9 2 5 6 4 5 9 2 1 4
7 6 10 1 2 5 10 9
1 9
 *)
open Scanf
open Printf

let lmax =
  List.fold_left (fun acc x -> if x > acc then x else acc) (-1000000)

let lmin =
  List.fold_left (fun acc x -> if x < acc then x else acc) 1000000

let main =
  try
    let n = ref 0 in
    while true do 
      n := !n + 1;
      scanf "%d" (fun k ->
        let vs = ref [] in
        for i = 1 to k do 
          scanf " %d"
            (fun v ->
              vs := v :: !vs) 
        done;
        scanf "\n" ();
        let min = lmin !vs in
        let max = lmax !vs in
        printf "Case %d: %d %d %d\n" !n min max (max - min)
      )
    done
  with _ ->
    () 
