package eu.timepit

import scala.meta._

package object konstant {

  // change to Either[String, Any] and return error msg on the Left
  def compileTimeConstant(term: Term): Option[Any] =
    term match {
      // TODO: Set, Vector, Map, Option, Array, Function0
      case Lit(value) =>
        Some(value)

      case q"(..$xs)" =>
        val args = xs.map(argToTerm).map(compileTimeConstant)
        sequence(args).map(seqToTuple)

      case q"List.empty" | q"Nil" =>
        Some(List())

      case q"List(..$xs)" =>
        val args = xs.map(argToTerm).map(compileTimeConstant)
        sequence(args)

      case _ =>
        None
    }

  private def argToTerm(arg: Term.Arg): Term =
    arg match {
      case term: Term => term
    }

  private def seqToTuple(list: Seq[Any]): Product = {
    val clazz = Class.forName(s"scala.Tuple${list.size}")
    val ctor = clazz.getConstructors.apply(0)
    val objects = list.map(_.asInstanceOf[Object])
    ctor.newInstance(objects: _*).asInstanceOf[Product]
  }

  private def sequence[A](seq: Seq[Option[A]]): Option[Seq[A]] =
    seq.foldRight(Option(List.empty[A])) { (optA, optAs) =>
      optA.flatMap(a => optAs.map(a :: _))
    }
}
