package ru.ponkin.logic

import org.scalacheck._
import org.scalacheck.Prop.forAll

object PredicateOpsCheck extends Properties("Evaluation"){

  val genFullTree = for {
    size <- Gen.choose(0, 500)
    tree <- genTree(size)
  } yield tree

  def genTree(maxDepth: Int): Gen[Predicate] = 
      if (maxDepth == 0) leaf else Gen.oneOf(leaf, genAnd(maxDepth), genOr(maxDepth), genNot(maxDepth))

  def genAnd(maxDepth: Int): Gen[Predicate] = for {
      depthL <- Gen.choose(0, maxDepth - 1)
      depthR <- Gen.choose(0, maxDepth - 1)
      left <- genTree(depthL)
      right <- genTree(depthR)
  } yield And(left, right)

  def genOr(maxDepth: Int): Gen[Predicate] = for {
      depthL <- Gen.choose(0, maxDepth - 1)
      depthR <- Gen.choose(0, maxDepth - 1)
      left <- genTree(depthL)
      right <- genTree(depthR)
  } yield Or(left, right)

  def genNot(maxDepth: Int): Gen[Predicate] = for {
      depth <- Gen.choose(0, maxDepth - 1)
      expr <- genTree(depth)
  } yield Not(expr)

  def leaf: Gen[Predicate] = Gen.oneOf(True, False)

  property("toDNF") = forAll(genFullTree){ expr =>
    val dnf = (PredicateOps.toNNF _ andThen PredicateOps.toDNF _).apply(expr)
    PredicateOps.eval(dnf) == PredicateOps.eval(expr)
  }
  

}
