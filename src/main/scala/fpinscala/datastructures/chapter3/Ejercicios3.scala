package fpinscala.datastructures.chapter3

/**
 * Created by mikelsanvicente on 11/01/16.
 */
class Ejercicios3 {

}

object Ejercicios3 {
  def sum1(l:List[Int]) : List[Int] = {
    l match {
      case Nil => Nil
      case Cons(v, tail) => Cons(v+1, sum1(tail))
    }
  }

  def toString(l:List[Double]) : List[String] = {
    l match {
      case Nil => Nil
      case Cons(v, tail) => Cons(v.toString , toString(tail))
    }
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    as match {
      case Nil => Nil
      case Cons(v, tail) => Cons(f(v) , map(tail)(f))
    }
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case Cons(v, tail) => if(!f(v)) filter(tail)(f) else Cons(v , filter(tail)(f))
    }
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    as match {
      case Nil => Nil
      case Cons(v, tail) => addtail(f(v), flatMap(tail)(f))
    }
  }

  def addtail[A](as: List[A], add:List[A]) : List[A] = {
    as match {
      case Nil => add
      case Cons(v, t) => Cons(v, addtail[A](t,add))
    }
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap[A,A](as)(a =>  if(f(a)) Cons(a,Nil) else Nil)
  }

  def sumLists(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1,l2) match {
      case (Cons(v1,tail1), Cons(v2,tail2)) => Cons(v1+v2,sumLists(tail1,tail2))
      case _ => Nil
    }
  }

  def zipWith[A,B](l1: List[A], l2: List[A])(f:(A,A)=>B): List[B] = {
    (l1,l2) match {
      case (Cons(v1,tail1), Cons(v2,tail2)) => Cons(f(v1,v2),zipWith(tail1,tail2)(f))
      case _ => Nil
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    if(startsWith(sup,sub)) {
      true
    } else {
      sup match {
        case Nil => sub == Nil
        case Cons(v, tail) => hasSubsequence(tail, sub)
      }
    }
  }

  def startsWith[A](sup: List[A], sub: List[A]) : Boolean = {
    (sup,sub) match {
      case (_, Nil) => true
      case (Cons(v1,tail1), Cons(v2,tail2)) => if(v1 == v2) startsWith(tail1,tail2) else false
      case (_,_) => false
    }
  }

  def size[A](t: Tree[A]):Int = {
    t match {
      case Leaf(_) => 1
      case Branch(left,right) => 1 + size(left) + size(right)
    }
  }

  def maximum(t: Tree[Int]):Int = {
    t match {
      case Leaf(v) => v
      case Branch(left,right) => maximum(left).max(maximum(right))
    }
  }

  def depth[A](t: Tree[A]):Int = {
    t match {
      case Leaf(v) => 1
      case Branch(left,right) => 1 + depth(left).max(depth(right))
    }
  }

  def map[A,B](t: Tree[A])(f : A=>B):Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(left,right) => Branch(map(left)(f),map(right)(f))
    }
  }

  def fold[A,B](t: Tree[A])(f: A => B, cons: (B ,B) => B): B = {
    t match {
      case Leaf(v) => f(v)
      case Branch(left,right) => cons(fold(left)(f,cons), fold(right)(f,cons))
    }
  }

  def size1[A](t: Tree[A]):Int = {
    fold[A,Int](t)( v=> 1 , _ + _)
  }

  def maximum1(t: Tree[Int]):Int = {
    fold[Int,Int](t)( v=> v , _ max _)
  }

  def depth1[A](t: Tree[A]):Int = {
    fold[A,Int](t)( v=> 1 , 1 +  _.max(_))
  }

  def map1[A,B](t: Tree[A])(f : A=>B):Tree[B] = {
    fold[A,Tree[B]](t)( (v:A) => Leaf(f(v)) , Branch(_, _) )
  }
}
