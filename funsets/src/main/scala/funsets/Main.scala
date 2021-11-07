package funsets

object Main extends App:
  import FunSets.*
  val s2 = singletonSet(2)
  val s1 = singletonSet(1)
  val s = union(s1, s2)
  println(contains(map(s,x=>x*x),4))
