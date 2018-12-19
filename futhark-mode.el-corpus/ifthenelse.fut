let foo () =
  if x then x else x

let foo () =
  if x
  then x
  else x

let foo () =
  if x
  then x
  else if x
       then x
       else x

let foo () =
  if x then x
  else if x then x
       else x

let foo () =
  if a < b
  then if a < c then a else c
  else if b < c then b else c
