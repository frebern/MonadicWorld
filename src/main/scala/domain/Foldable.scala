package domain

trait Foldable[F[_]]{ //아 망할 폴더블 왜 인스턴스를 못만드냐;;

  import Monoid._

  // B가 Monoidal 하다면 foldMap 할 수 있다.
  def foldMap[A, B](fa: F[A])(f: A => B)(implicit mb: Monoid[B]) : B

  // A가 Monoidal 하다면 fold 할 수 있다.
  def fold[A](fa: F[A])(implicit m: Monoid[A]) : A = foldMap(fa)(identity)

  def foldRight[A, B](fa: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(fa)(f.curried)(endoMonoid[B])(z)
  def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(fa)(a => (b:B) => f(b, a))(dual(endoMonoid[B]))(z)

}
