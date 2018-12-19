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

let foo () =
  let x =
    if a then b else
    let x = 2 in x
  in x

let foo () =
  if x then y else
  if x then y else
  x
