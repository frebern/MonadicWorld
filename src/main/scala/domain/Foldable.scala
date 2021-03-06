package domain

trait Foldable[F[_ <: Object]]{

  import Monoid._

  // B가 Monoidal 하다면 foldMap 할 수 있다.
  def foldMap[A <: Object, B](fa: F[A])(f: A => B)(implicit mb: Monoid[B]) : B

  // A가 Monoidal 하다면 fold 할 수 있다.
  def fold[A <: Object](fa: F[A])(implicit m: Monoid[A]) : A = foldMap(fa)(identity)

  def foldRight[A <: Object, B](fa: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(fa)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A <: Object, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(fa)(a => (b:B) => f(b, a))(dual(endoMonoid[B]))(z)

}
