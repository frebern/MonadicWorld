package domain

trait Object
case class Node(color:String) extends Object

trait Collection extends Object {
  def isEmpty:Boolean
  def length:Int
}

trait Box[+T] extends Collection {
  def ++[S >: T <: Object](right:Box[S]):Box[S]
}
case class ItemsInBox[+T <: Object](items : T*) extends Box[T] {
  override def ++[S >: T <: Object](right: Box[S]): Box[S] =
    right match {
      case ItemsInBox(rightItems @ _*) => ItemsInBox(items ++ rightItems :_*)
      case EmptyBox => this
    }
  override def isEmpty: Boolean = items.isEmpty
  override def length: Int = items.length

  override def toString: String =  "Box" + items.mkString("(", ", ", ")")
}
case object EmptyBox extends Box[Nothing] {
  override def ++[S >: Nothing <: Object](right: Box[S]): Box[S] = right
  override def isEmpty: Boolean = true
  override def length: Int = 0
}

trait Group[+T] extends Collection {
  def ++[S >: T <: Object](right:Group[S]):Group[S]
}
case class ItemsInGroup[+T<:Object](items : T*) extends Group[T] {
  override def ++[S >: T <: Object](right: Group[S]): Group[S] =
    right match {
      case ItemsInGroup(rightItems @ _*) => ItemsInGroup(items ++ rightItems :_*)
      case EmptyGroup => this
    }
  override def isEmpty: Boolean = items.isEmpty
  override def length: Int = items.length

  override def toString: String =  "Group" + items.mkString("(", ", ", ")")
}
case object EmptyGroup extends Group[Nothing] {
  override def ++[S >: Nothing <: Object](right: Group[S]): Group[S] = right
  override def isEmpty: Boolean = true
  override def length: Int = 0
}

object Box {

  def apply[T <: Object](items:T*): Box[T] =
    if(items.isEmpty) EmptyBox
    else ItemsInBox(items:_*)

  implicit def monoid[T <: Object] = new Monoid[Box[T]]{
    override def op(left: Box[T], right: Box[T]): Box[T] = left ++ right
    override def zero: Box[T] = EmptyBox
  }

  def foldable = new Foldable[Box] {
    override def foldMap[A, B](fa: Box[A])(f: A => B)(implicit mb: Monoid[B]): B =
      fa match {
        case ItemsInBox(items @ _*) => items.foldLeft(mb.zero)((acc, i) => mb.op(f(i), acc))
        case EmptyBox => mb.zero
      }
  }

  def monad = new Monad[Box] {
    override def unit[A <: Object](a: => A): Box[A] = Box(a)
    override def flatMap[A <: Object, B <: Object](ma: Box[A])(f: A => Box[B]): Box[B] =
      ma match {
        case ItemsInBox(x, xs @ _*) => f(x) ++ flatMap(Box(xs :_*))(f)
        case _ => EmptyBox
      }
  }

  def functor = new Functor[Box] {
    override def map[A <: Object, B <: Object](fa: Box[A])(f: A => B): Box[B] =
      fa match {
        case ItemsInBox(items @ _*) => ItemsInBox(items.map(f) :_*)
        case _ => EmptyBox
      }
  }

}

object Group {

  def apply[T <: Object](items:T*): Group[T] =
    if(items.isEmpty) EmptyGroup
    else ItemsInGroup(items:_*)

  implicit def monoid[T <: Object] = new Monoid[Group[T]]{
    override def op(left: Group[T], right: Group[T]): Group[T] = left ++ right
    override def zero: Group[T] = EmptyGroup
  }

  def foldable = new Foldable[Group] {
    override def foldMap[A, B](fa: Group[A])(f: A => B)(implicit mb: Monoid[B]): B =
      fa match {
        case ItemsInGroup(items @ _*) => items.foldLeft(mb.zero)((acc, i) => mb.op(f(i), acc))
        case EmptyGroup => mb.zero
      }
  }

  def monad = new Monad[Group] {
    override def unit[A <: Object](a: => A): Group[A] = Group(a)
    override def flatMap[A <: Object, B <: Object](ma: Group[A])(f: A => Group[B]): Group[B] =
      ma match {
        case ItemsInGroup(x, xs @ _*) => f(x) ++ flatMap(Group(xs :_*))(f)
        case _ => EmptyGroup
      }
  }

  def functor = new Functor[Group] {
    override def map[A <: Object, B <: Object](fa: Group[A])(f: A => B): Group[B] =
      fa match {
        case ItemsInGroup(items @ _*) => ItemsInGroup(items.map(f) :_*)
        case _ => EmptyGroup
      }
  }

}