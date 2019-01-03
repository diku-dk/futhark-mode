module m(real: real): {
  val foo:
    (screenX: i32) -> (screenY: i32) ->
    (xcentre: real.t) -> (ycentre: real.t) -> (width: real.t) ->
    (limit: i32) -> (radius: real.t) ->
    [screenX][screenY]i32
} = { }
