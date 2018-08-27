package domain

trait Functor[F[_ <: Object]] {
  def map[A <: Object, B <: Object](fa: F[A])(f: A => B) : F[B]
}
