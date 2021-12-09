def f (x : #foo | #bar) : #foo | #bar =
  match x
  case #foo ->
    #bar
  case #bar -> let x = 2
               in x
