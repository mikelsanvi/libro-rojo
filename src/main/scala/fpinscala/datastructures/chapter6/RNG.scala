package fpinscala.datastructures.chapter6

/**
 * Created by mikelsanvicente on 8/03/16.
 */
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
      (n, nextRNG)
  }

}

object Test {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (number,rng1) = rng.nextInt

    if(number == Int.MinValue)
      nonNegativeInt(rng1)
    else
      (if(number < 0) number * -1 else number, rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (number, rng1) = nonNegativeInt(rng)
    if(number == Int.MaxValue)
      double(rng1)
    else
      ((number.toDouble / Int.MaxValue).toDouble, rng1)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (number1, rng1) = nonNegativeInt(rng)
    val (number2, rng2) = double(rng1)
    ((number1,number2),rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (number1, rng1) = double(rng)
    val (number2, rng2) = nonNegativeInt(rng1)
    ((number1,number2),rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (number1, rng1) = double(rng)
    val (number2, rng2) = double(rng1)
    val (number3, rng3) = double(rng2)
    ((number1,number2,number3),rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count > 0) {
      val (list, rng1) = ints(count -1)(rng)
      val (number, rng2) = nonNegativeInt(rng1)
      (number :: list, rng2)
    } else {
      (Nil,rng)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }


  def doubleMap: Rand[Double] = {
    map(nonNegativeInt _)(int => ( (int.toDouble + 1) / Int.MaxValue).toDouble)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a,rng1) = ra(rng)
      val (b,rng2) = rb(rng1)
      (f(a,b),rng2)
    }
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt _)(i => i - i % 2)

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val int: Rand[Int] = _.nextInt

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)
  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs match {
      case r :: tail =>
        RNG => {
          val head = r(RNG)
          val t = sequence(tail)(head._2)
          val list = head._1 :: t._1
          (list,t._2)
        }
      case Nil => {
        RNG => (Nil,RNG)
      }
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    RNG => {
      val (a, rng1) = f(RNG)
      g(a)(rng1)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt _)(a => {
      val mod = a % n
      if (a + (n-1) - mod >= 0)
        rng => (mod, rng)
      else nonNegativeLessThan(n)
    })
  }

  def mapFlat[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => rnd => (f(a),rnd))
  }

  def map2Flat[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => rnd => (f(a,b),rnd)))
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)


  //type State[S,+A] = S => (A,S)


}

case class State[S,+A](run: S => (A,S)) {
  def map[B](f: A => B): State[S,B] = {
    this.flatMap(a => State.unit(f(a)))
  }

  def flatMap[B](g: A => State[S,B]): State[S,B] = {
    State(s => {
      val (a,s1) = run(s)
      g(a).run(s1)
    })
  }

  def map2[B,C]( rb: State[S,B])(f: (A, B) => C): State[S,C] = {
    this.flatMap[C](a => rb.flatMap[C](b => State.unit[C,S](f(a,b))))
  }
}

object State {
  def unit[A,S](a:A): State[S,A] =
    State(s => (a, s))

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] = {
    fs match {
      case r :: tail =>
        r.flatMap[List[A]](a =>
          sequence(tail).flatMap( tail => unit(a::tail))
        )
      case Nil => {
        unit(Nil)
      }
    }
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyTest {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- State.sequence(inputs map (State.modify[Machine] _ compose update))
      s <- State.get
    } yield (s.coins, s.candies)
}



