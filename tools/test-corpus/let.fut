def f0 () =
  let x = 0
  in x

def f1 () =
  let x = 0 in
  x

def f2 () =
  let x = 0
  let y = 0
  in x

def f3 () =
  let x = 0
  let y = 0 in
  x

def f4 () =
  let x = 0 in
  let y = 0 in
  x

def f5 () =
  let x =
    0

def f6 () =
  let x = 0 in
  let y =
    let x = 0
    let y = 0
    in 0
  let z = 0
  in 0

def f7 () =
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

def f8 () =
  let (x, y) = (1, 2)
  let z = x + 3
  in z

-- A comment.
def f9 () = x
