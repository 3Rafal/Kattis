open Scanf
open Printf

let go () = 
  try
    scanf "%d+%d\n" (fun x y -> printf "%d\n" (x + y))
  with Scan_failure _ ->
    scanf "%s\n" (fun _ -> printf "skipped\n")

let main =
  scanf "%d\n" (fun n ->
      for i = 1 to n do
        go ()
      done)
