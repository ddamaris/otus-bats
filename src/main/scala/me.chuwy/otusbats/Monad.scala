package me.chuwy.otusbats

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(a => a)
}

case class User[T](anyFiled:T)

object Monad {

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    override def point[A](a: A): Option[A] = Option(a)
  }

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    override def point[A](a: A): List[A] = List(a)
  }

  implicit val userMonad: Monad[User] = new Monad[User] {
    override def map[A, B](fa: User[A])(f: A => B): User[B] = User(f(fa.anyFiled))
    override def flatMap[A, B](fa: User[A])(f: A => User[B]): User[B] = f(fa.anyFiled)
    override def point[A](a: A): User[A] = User(a)
  }

  implicit class MonadOps[F[_], A](fa: F[A]) {
    def flatMap[B](f: A => F[B])(implicit md: Monad[F]): F[B] = md.flatMap(fa)(f)
    def map[B](f: A => B)(implicit md: Monad[F]): F[B] = md.map(fa)(f)
  }
}

object MonadApp extends App{
  import Monad._
  val t1 = User(5)
  println(t1.map(_ + 5))

  val t2 = User("ab")
  println(t2.map(_ + "cd"))
}