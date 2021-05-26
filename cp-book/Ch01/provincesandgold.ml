open Scanf
open Printf

let count l r = (max l r * 2)

let victoryCard n =
  if n >= 8 then Some "Province"
  else if n >= 5 then Some "Duchy"
  else if n >= 2 then Some "Estate"
  else None

let treasureCard n =
  if n >= 6 then "Gold"
  else if n >= 3 then "Silver"
  else "Copper"

let main =
  scanf "%d %d %d\n" (fun g s c ->
      let power = 3 * g + 2 * s + c in
      let treasure = treasureCard power in
      match victoryCard power with
      | Some x -> printf "%s or %s\n" x treasure
      | None -> printf "%s\n" treasure
    )
