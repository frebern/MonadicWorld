package domain

// A monad is just a monoid in the category of endofunctors.

trait Monad[F[_]] extends Functor[F]{
  def unit[A](a: => A) :F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]) : F[B]

  // Free
  def map[A, B](ma: F[A])(f: A => B) : F[B] = flatMap(ma)(a => unit(f(a)))
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C) : F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))

//  // Need Foldable
//  def traverse[FF[_] <: Foldable, A, B](la: FF[A])(f: A => F[B]): F[FF[B]] =
//    Foldable.fold()
//  def sequence[FF[_] <: Foldable, A](lma: FF[F[A]]): F[FF[A]] =
//    traverse(lma)(identity)


}
