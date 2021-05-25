open Scanf
open Printf

let main =
  scanf "%[a-zA-z ]\n" (fun name ->
          printf "Thank you, %s, and farewell!" name)
