let f = \x ->
          x + 2

let g = map (\x ->
               let y = 2
               in x + y)
            [1,2,3]

let h = map (\(x) ->
               x)

let f = map (\x ->
               x ||
               f)
