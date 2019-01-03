let f (x : #foo) =
  match x
  case #foo ->
    match x
    case x ->
      2
    case y ->
      match y
      case #abc ->
        5
      case #def ->
        6
