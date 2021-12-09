module B {
  module A {
    include dog

    type a = int

    val t = 3

    def main () : int = 0

    entry cat () : int = 0
  }

  val u = 4

  module C {
    type c = f32
  }
}
