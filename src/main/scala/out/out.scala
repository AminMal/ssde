package out

type Id[A] = A

trait Applicative[F[_]] {
  def pure[T](t: T): F[T]
}

trait Functor[F[_]] {
  def map[A, B](value: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Applicative[F] with Functor[F] {
  def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]

  def map[A, B](value: F[A])(f: A => B): F[B] =
    flatMap(value)(x => pure(f(x)))
}

given Applicative[Id] = new Applicative[Id] {
  override inline def pure[T](t: T): Id[T] = t
}

given Functor[Id] = new Functor[Id] {
  override inline def map[A, B](value: Id[A])(f: A => B): Id[B] = f(value)
}

given Monad[Id] = new Monad[Id] {
  override inline def flatMap[A, B](value: Id[A])(f: A => Id[B]): Id[B] = f(value)

  override inline def pure[T](t: T): Id[T] = summon[Applicative[Id]].pure(t)
}