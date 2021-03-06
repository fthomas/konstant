package eu.timepit.konstant

import org.scalacheck.Prop._
import org.scalacheck.{Prop, Properties}
import scala.meta._

class PackageSpec extends Properties("konstant") {

  def isConstant[T](term: Term, value: T): Prop =
    secure(compileTimeConstant(term) ?= Some(value))

  def nonConstant(term: Term): Prop =
    secure(compileTimeConstant(term) ?= None)

  property("()") = isConstant(q"()", ())

  property("1") = isConstant(q"1", 1)

  property("3.14") = isConstant(q"3.14", 3.14)

  property(""""hello"""") = isConstant(q""""hello"""", "hello")

  property("List(1)") = isConstant(q"List(1)", List(1))

  property("List(2)") = isConstant(q"List(2)", List(2))

  property("List(1, 2, 3)") = isConstant(q"List(1, 2, 3)", List(1, 2, 3))

  property("List()") = isConstant(q"List()", List())

  property("Nil") = secure {
    val term = "Nil".parse[Term].get
    val value = List()
    compileTimeConstant(term) ?= Some(value)
  }

  property("My List") = secure {
    case class List()

    val term = "List()".parse[Term].get
    val value = List()
    compileTimeConstant(term) != Some(value)
  }

  property("List.empty") = isConstant(q"List.empty", List.empty)

  property("(1, 2)") = isConstant(q"(1, 2)", (1, 2))

  property("""(1, "2", 3.0)""") = isConstant(q"""(1, "2", 3.0)""", (1, "2", 3.0))

  //property("0 -> 1") = isConstant(q"0 -> 1", 0 -> 1)

  //property("Some(value = 1)") = isConstant(q"Some(value = 1)", Some(value = 1))

  property("x") = nonConstant(q"x")

  property("List(x)") = nonConstant(q"List(x)")

  property("List(1, x)") = nonConstant(q"List(1, x)")

  property("""List({ println(""); 1})""") = nonConstant(q"""List({ println(""); 1})""")
}
