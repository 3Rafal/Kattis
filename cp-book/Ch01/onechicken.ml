open Scanf
open Printf

let pcsStr n =
  if (abs n) = 1 then "piece"
  else sprintf "pieces"

let main =
  scanf "%d %d\n" (fun ppl pcs ->
      let num = pcs - ppl in
      let pcs = pcsStr num in
      if num >= 0 then
          printf "Dr. Chaz will have %d %s of chicken left over!\n" num pcs
      else
          printf "Dr. Chaz needs %d more %s of chicken!\n" (abs num) pcs)
