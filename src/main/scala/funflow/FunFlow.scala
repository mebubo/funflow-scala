package funflow

import cats.arrow.{Arrow, Category}
import cats.effect.IO

object FunFlow {

  sealed trait Flow[A, B]
  final case class Step[A, B](f: A => IO[B]) extends Flow[A, B]
  final case class Name[A, B](s: String, f: Flow[A, B]) extends Flow[A, B]
  final case class Compose[A, B, C](fab: Flow[A, B], fbc: Flow[B, C]) extends Flow[A, C]
  final case class First[A, B, C](fbc: Flow[A, B]) extends Flow[(A, C), (B, C)]

  implicit val flowCategory: Category[Flow] = new Category[Flow] {
    override def id[A]: Flow[A, A] = Step(IO.pure)
    override def compose[A, B, C](f: Flow[B, C], g: Flow[A, B]): Flow[A, C] =
      Compose(g, f)
  }

  implicit val flowArrow: Arrow[Flow] = new Arrow[Flow] {
    override def lift[A, B](f: A => B): Flow[A, B] = Step(f.andThen(IO.pure))
    override def first[A, B, C](fa: Flow[A, B]): Flow[(A, C), (B, C)] = First(fa)
    override def compose[A, B, C](f: Flow[B, C], g: Flow[A, B]): Flow[A, C] =
      Compose(g, f)
  }

  def runFlow[A, B, C, D](fab: Flow[A, B], a: A): IO[B] = fab match {
    case Step(f) => f(a)
    case Name(_, f)  => runFlow(f, a)
    case Compose(f, g) =>
      for {
        b <- runFlow(f, a)
        x <- runFlow(g, b)
      } yield x
    case First(f) =>
      // scala GADT?
      val (x, d) = a
      for {
        y <- runFlow(f, x)
      } yield (y, d)
  }

  def promptFor: Flow[String, String] = ???
}
