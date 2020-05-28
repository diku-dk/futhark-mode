let res1 =
  #[attr]
  map f
  xs -- FIXME: not what I would prefer.

let res2 = #[attr]
           map f
           xs

let res3 =
  #[attr] map f xs
