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
  then x else x

let foo () =
  if x then x
  else if x then x
  else x

let foo () =
  if a < b
  then if a < c then a else c
  else if b < c then b else c

let foo () =
  if a < b
  then if a < c
       then a
       else c
  else if b < c
  then b
  else c

let foo () =
  let x =
    if a then b else
    let x = 2 in x
  in x

let foo () =
  if x then y else
  if x then y else
    x

let foo () =
  if x
  then y
  else
  let z = 3
  in z

let foo () =
  if x then y
  else if x then y
  else if x then z
  else 0

let foo () =
  if x
  then y
  else if x
  then if a
       then b
       else c
  else if x
  then z
  else 0

let foo () =
  if cond
  then
  let cost[id] = cost[tid] + 1
  let updating_graph_mask[id] = true
  in (cost, updating_graph_mask)
  else baz
