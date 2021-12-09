def f x =
  unsafe
  let x = 2 in x

def f =
  let c1 = if true
           then x * unsafe x
           else x
  let c2 = x
  in x
