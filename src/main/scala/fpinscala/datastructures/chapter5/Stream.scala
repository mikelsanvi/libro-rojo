package fpinscala.datastructures.chapter5

import Stream._

/**
 * Created by mikelsanvicente on 19/01/16.
 */
sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }
  def toListRecursive: List[A] = {
    this match  {
      case Empty => Nil
      case Cons(h,t) => h() :: t().toListRecursive
    }
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }

  @annotation.tailrec
  final def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a,b)=> p(a) && b)
  }

  def takeWhile2(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a,b) => cons(a,b.takeWhile2(f) ))
  }

  def headOptionFold: Option[A] = {
    foldRight[Option[A]](None)((a,b)=> Some(a)).map(v => v)
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a,b)=> cons(f(a), b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a,b) => if(f(a)) cons(a,b) else b)
  }

  def append[AA >: A](a: =>Stream[AA]): Stream[AA] = {
    foldRight(a)((a,b)=> cons(a,b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h,t) => f(h).append(t))
  }

  def mapUnfold[B](f: A => B): Stream[B] = {
    unfold(this)(stream => {
      stream match {
        case Empty => None
        case Cons(v,tail) => Some(f(v()),tail())
      }
    })
  }

  def takeUnfold(n: Int): Stream[A] = ???

  def takeWhileUnfold(f: A => Boolean): Stream[A] = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    cons(a,constant(a))
  }

  def from(n: Int): Stream[Int] = {
    cons(n,from(n+1))
  }

  def fibs(): Stream[Int] = {
    def next(v1:Int, v2:Int) :Stream[Int] = {
      val newValue= v1 + v2
      cons(newValue, next(v2,newValue))
    }
    cons(0,cons(1,next(0,1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).flatMap[Stream[A]](v => Some(cons(v._1,unfold(v._2)(f)))).getOrElse(empty)
  }

  def fibsUnfold(): Stream[Int] = {
    unfold[Int,(Int,Int)]((0,1))(s => Some(s._1, (s._2, s._1 + s._2)))
  }

  def fromUnfold(n: Int): Stream[Int] = {
    unfold[Int,Int](n)(s => Some(s, s + 1))
  }

  def constantUnfold[A](a: A): Stream[A] = {
    unfold[A,A](a)(s => Some(a, a))
  }

}