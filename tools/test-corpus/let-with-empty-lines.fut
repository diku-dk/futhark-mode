let foo () =
  let foo () = 3
  let foo = 3
  in foo


-- The same, but with some empty lines:

let foo () =

  let foo () = 3

  let foo = 3
  in foo
