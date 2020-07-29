package zone.slice.fpinscala.chapter7

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

sealed trait Future[+A] {
  private[chapter7] def apply(cb: A => Unit): Unit
}

object Future {
  def apply[A](f: (A => Unit) => Unit): Future[A] =
    new Future[A] {
      def apply(cb: A => Unit): Unit = f(cb)
    }
}

object Par {

  type Par[+A] = ExecutorService => Future[A]

  /**
    * Obtains the value from a parallel computation, performing any work
    * necessary to compute the final output value.
    */
  def run[A](es: ExecutorService)(pa: Par[A]): A = {
    val ref   = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    pa(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }

  /**
    * Submits a computation to be executed through an `ExecutorService`.
    */
  private[chapter7] def submit(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  /**
    * Promotes a constant value to a parallel computation.
    *
    * The value will be evaluated immediately.
    */
  def pure[A](a: A): Par[A] =
    _ => Future(_(a))

  /**
    * A non-strict version of `pure`.
    *
    * Keep in mind that this function simply delays the evaluation of the
    * provided value. To perform a computation off of the main thread, use
    * `lazyUnit` (which uses `fork`).
    */
  def delay[A](a: => A): Par[A] =
    _ => Future(_(a))

  /**
    * Explicitly desigates a computation to be performed off of the main
    * thread.
    */
  def fork[A](pa: => Par[A]): Par[A] =
    es => Future { cb => submit(es)(pa(es)(cb)) }

  /**
    * Promotes a constant value to a parallel computation that will be evaluated
    * off of the main thread.
    */
  def lazyUnit[A](a: => A): Par[A] = fork(pure(a))

  /**
    * Makes a function evaluate its result asynchronously.
    */
  // Exercise 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = { a =>
    lazyUnit(f(a))
  }

  // Exercise 7.1
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es =>
      Future { cb =>
        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = Actor[Either[A, B]](es) {
          case Left(a) =>
            br match {
              case None    => ar = Some(a)
              case Some(b) => submit(es)(cb(f(a, b)))
            }

          case Right(b) =>
            ar match {
              case None    => br = Some(b)
              case Some(a) => submit(es)(cb(f(a, b)))
            }
        }

        pa(es)(a => combiner ! Left(a))
        pb(es)(b => combiner ! Right(b))
      }

  /**
    * Transforms a value inside of a `Par`.
    */
  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, pure())((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  /**
    * Maps over a `List[A]` asynchronously.
    */
  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = {
    var pbs: List[Par[B]] = as.map(asyncF(f))
    sequence(pbs)
  }

  // Exercise 7.5
  def sequence[A](pas: List[Par[A]]): Par[List[A]] =
    pas match {
      case Nil => pure(Nil)
      // fork(..) here so that it's tail recursive (:
      case h :: t => map2(h, fork(sequence(t)))(_ :: _)
    }

  // Exercise 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    as match {
      case Nil => Par.pure(Nil)
      case h :: t =>
        if (f(h)) map2(pure(h), parFilter(t)(f))(_ :: _) else parFilter(t)(f)
    }

  def choice[A](predicate: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      Future { cb =>
        predicate(es) { b =>
          if (b) submit(es) { t(es)(cb) }
          else submit(es) { f(es)(cb) }
        }
      }
}

object ParUse {
  def plainSum(ints: Seq[Int]): Int =
    ints.foldLeft(0)(_ + _)

  def divideAndConquerSum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      divideAndConquerSum(l) + divideAndConquerSum(r)
    }

  import Par.Par

  def parSum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.pure(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(parSum(l)), Par.fork(parSum(r)))(_ + _)
    }
}
