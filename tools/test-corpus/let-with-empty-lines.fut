def foo () =
  let foo () = 3
  let foo = 3
  in foo


-- The same, but with some empty lines:

def foo () =

  let foo () = 3

  let foo = 3
  in foo
