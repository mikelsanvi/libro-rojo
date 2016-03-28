package fpinscala.datastructures.chapter4

/**
 * Created by mikelsanvicente on 13/01/16.
 */
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(v) => Some(f(v))
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(v) => v
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(v => if(f(v)) Some(v) else None)
  }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Ejercicios4 {

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(a => b.flatMap(b => Try(f(a,b))))
  }

  def map2For[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      a <- a
      b <- b
    } yield (f(a,b))
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try { age.toInt }
    val optTickets: Option[Int] = Try { numberOfSpeedingTickets.toInt }
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case a :: tail => a.flatMap(a => sequence(tail).flatMap( tail => Some(::(a,tail))))
    }
  }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    as match {
      case Nil => Some(Nil)
      case a :: tail => map2(f(a), traverse(tail)(f))(_ :: _)
    }
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(a => a.map( a => a))
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }
}