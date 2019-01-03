module type SIG = {
  type t

  val inject: i32 -> i32 -> t
  val extract: t -> (i32,i32)
  val f: []t -> t
}
