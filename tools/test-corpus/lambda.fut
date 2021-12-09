def f = \x ->
          x + 2

def g = map (\x ->
               let y = 2
               in x + y)
            [1,2,3]

def h = map (\(x) ->
               x)

def f = map (\x ->
               x ||
               f)

def f = map (\x
              y ->
               y + x)
