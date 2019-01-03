let f (n: i32) =
  let n = loop n for i < n do n + 1
  in n

let g (n: i32) =
  let n = loop n for i < n do n + 1
  let n = n + 2
  in n

let h (n: i32) =
  let n = loop n for i < n do
          let n = n + 1
          in n
  let n = n + 2
  in n

let f' (n: i32) =
  let n =
    loop n for i < n do n + 1
  in n

let g' (n: i32) =
  let n =
    loop n for i < n do n + 1
  let n = n + 2
  in n

let h' (n: i32) =
  let n =
    loop n for i < n do
    let n = n + 1
    in n
  let n = n + 2
  in n

let h'' (n: i32) =
  loop x for _i < n do
    advance
