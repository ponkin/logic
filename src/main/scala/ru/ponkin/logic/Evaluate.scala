package ru.ponkin.logic

object Evaluate{

  def main(args: Array[String]) = {
    val expr = And(False, Or(And(Or(True, True), False), True))
    val dnf = (PredicateOps.toDNF _ andThen PredicateOps.toNNF _).apply(expr)
    println(dnf)
    println( PredicateOps.traverse(dnf) )
  }
}
