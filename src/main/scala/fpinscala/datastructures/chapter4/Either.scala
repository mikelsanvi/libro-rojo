package fpinscala.datastructures.chapter4

import scala.util.Try


/**
 * Created by mikelsanvicente on 18/01/16.
 */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this.flatMap(a => Right(f(a)))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match{
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match{
      case Left(_) => b
      case Right(a) => Right(a)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      a <- this
      b <- b
    } yield(f(a,b))
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object EjerciciosEither {
  def Try[A](a: => A): Either[Exception, A] = {
    try
      Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(_.map(a => a))
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      case v :: tail => f(v).map2(traverse(tail)(f))(_ :: _)
    }
  }
}