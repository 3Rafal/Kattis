open Scanf
open Printf

let process sweet sour =
  if sweet + sour = 13 then "Never speak again."
  else if sweet < sour then "Left beehind."
  else if sweet = sour then "Undecided."
  else "To the convention."
  
let main =
  try
    while true do 
      scanf "%d %d\n" (fun sweet sour ->
          if sweet = 0 && sour = 0 then failwith "zeros"
          else printf "%s\n" (process sweet sour)
        )
    done
  with _ -> ()
