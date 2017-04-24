package eu.timepit.konstant

import scala.meta._

object Konstant {

  // TODO: always prefix with _root_

  def compileTimeConstant(term: Term): Option[Any] =
    term match {
      case Lit(value) => Some(value)
      case q"List()" | q"List.empty" | q"Nil" => Some(List())
      case q"List(..$xs)" =>
        val args = xs.map(argToTerm).map(compileTimeConstant)
        sequence(args.toList)
      case _ => None
    }

  def argToTerm(arg: Term.Arg): Term =
    arg match {
      case term: Term => term
    }

  def sequence[A](list: List[Option[A]]): Option[List[A]] =
    list.foldRight(Option(List.empty[A])) { (optA, optAs) =>
      optA.flatMap(a => optAs.map(a :: _))
    }

  /*

  def compileTimeConstant[T](t: c.Expr[T]): Option[T] =
    t.tree match {
      case Literal(Constant(value)) => Some(value.asInstanceOf[T])
      case x if isCompileTimeConstant(x) => Some(eval(t))
      case _ => None
    }

  def isCompileTimeConstant(t: Tree): Boolean =
    t match {
      case q"immutable.this.Nil" => true
      case q"$term.apply[$tpe](..$args)" =>
        standardCollections.exists(_ equalsStructure term) && args.forall(isLiteralConstant)
      case _ => false
    }

  val standardCollections: List[Tree] = List(
    q"immutable.this.List",
    q"scala.this.Predef.Set",
    q"scala.`package`.Vector",
    q"scala.collection.immutable.List",
    q"scala.collection.immutable.Set",
    q"scala.collection.immutable.Vector"
  )

  def isLiteralConstant(t: Tree): Boolean =
    PartialFunction.cond(t) { case Literal(Constant(_)) => true }

 */
}
