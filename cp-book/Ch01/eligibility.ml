open Scanf
open Printf

let go () = 
  scanf "%s %d/%[0-9/] %d/%[0-9/] %d\n"
    (fun name stYear _ birthYear _ courses ->
      if stYear >= 2010 || birthYear >= 1991 then
        printf "%s eligible\n" name
      else if courses > 40 then
        printf "%s ineligible\n" name
      else
        printf "%s coach petitions\n" name)

let main =
  scanf "%d\n" (fun n ->
      for i = 1 to n do
        go ()
      done)

