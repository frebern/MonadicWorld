package domain

trait Monoid[T] {
  def op(left: T, right: T) : T
  def zero : T
}

object Monoid {

  def dual[A](m: Monoid[A]):Monoid[A] = new Monoid[A] {
    override def op(left: A, right: A) : A = m.op(right, left)
    override def zero : A = m.zero
  }

  def endoMonoid[A] : Monoid[A => A] = new Monoid[A => A] {
    override def op(left: A => A, right: A => A) =  left compose right
    override def zero = (t: A) => t
  }

}