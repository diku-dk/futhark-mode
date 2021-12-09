module A = {
  type a = int

  include dog

  entry cat () : int = 0

  def main () : int = 0
}

module B = {
  def f6 () =
    let x = 0 in
    let y =
      let x = 0
      let y = 0
      in 0
    let z = 0
    in 0
}

module C = {
  open (F R)
  open B
}

local module D = {
  open import "test"
}
