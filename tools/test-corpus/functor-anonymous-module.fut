module A = SomeFunctor {
  type t = (int, int)

  def id (x : t) : t = x
}
