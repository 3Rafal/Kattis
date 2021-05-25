open Scanf
open Printf

let main =
  scanf "%s %d\n" (fun m d ->
      if (m = "OCT" && d = 31)
         || (m = "DEC" && d = 25)
      then
        printf "yup\n"
      else
        printf "nope\n")
