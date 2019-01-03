let f0 () =
  let x = 0
  in x

let f1 () =
  let x = 0 in
  x

let f2 () =
  let x = 0
  let y = 0
  in x

let f3 () =
  let x = 0
  let y = 0 in
  x

let f4 () =
  let x = 0 in
  let y = 0 in
  x

let f5 () =
  let x =
    0

let f6 () =
  let x = 0 in
  let y =
    let x = 0
    let y = 0
    in 0
  let z = 0
  in 0

let f7 () =
  let x = 0 in
  let y =
    let x = 0
    let y = let x = 0
            let y = 0
            in 0
    let z = 0
    in 0
  let z = 0
  in z

let f8 () =
  let (x, y) = (1, 2)
  let z = x + 3
  in z

-- A comment.
let f9 () = x
