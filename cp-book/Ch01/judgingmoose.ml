open Scanf
open Printf

let count l r = (max l r * 2)

let main =
  scanf "%d %d\n" (fun l r ->
      if (l = 0 && r = 0) then
        printf "Not a moose\n"
      else if (l = r) then
        printf "Even %d\n" (count l r)
      else
        printf "Odd %d\n" (count l r))
