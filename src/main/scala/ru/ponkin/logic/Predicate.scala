package ru.ponkin.logic

sealed trait Predicate
case class Or(left: Predicate, right: Predicate) extends Predicate
case class And(left: Predicate, right: Predicate) extends Predicate
case class Not(pred: Predicate) extends Predicate
case object True extends Predicate
case object False extends Predicate

object PredicateOps{

  def toNNF(pred: Predicate): Predicate = pred match {
    case a @ (True | False) => a
    case a @ Not( True | False) => a
    case Not(Not(p)) => p
    case And(l, r) => And(toNNF(l), toNNF(r))
    case Not(And(l, r)) => toNNF( Or(Not(l), Not(r)))
    case Or(l, r) => Or(toNNF(l), toNNF(r))
    case Not(Or(l,r)) => toNNF( And(Not(l), Not(r)))
  }

  def dist(predL: Predicate, predR: Predicate): Predicate = (predL, predR) match {
    case (Or(l, r), p) => Or(dist(l, p), dist(r, p))
    case (p, Or(l, r)) => Or(dist(p, l), dist(p, r))
    case (l, r) => And(l, r)
  }

  def toDNF(pred: Predicate): Predicate = pred match {
    case And(l, r) => dist(toDNF(l), toDNF(r))
    case Or(l, r) =>  Or(toDNF(l), toDNF(r))
    case p => p
  }

  def eval(pred: Predicate): Boolean = pred match {
    case And(l, r) => eval(l) && eval(r)
    case Or(l, r) => eval(l) || eval(r) 
    case Not(p) => !eval(p)
    case True => true
    case False => false
  }

  def traverse(pred: Predicate): List[Predicate] = pred match {
    case Or(l, r) => traverse(l) ++ traverse(r) 
    case a  => a :: Nil
  }
}
