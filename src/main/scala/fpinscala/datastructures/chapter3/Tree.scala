package fpinscala.datastructures.chapter3

/**
 * Created by mikelsanvicente on 13/01/16.
 */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]