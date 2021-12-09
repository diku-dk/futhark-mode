def f x =
  let y = match x case 0 -> 1
                  case _ -> 0
  in match y
     case 2 -> 3
     case _ -> 1
