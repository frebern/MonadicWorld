package domain

trait Object
case class Node(color:String) extends Object

trait Collection extends Object {
  def isEmpty:Boolean
  def length:Int
}

trait Box[+T] extends Collection {
  def ++[S >: T <: Object](box:Box[S]):Box[S] = this match {
    case ItemsInBox(leftItems @ _*) => box match {
      case ItemsInBox(rightItems @ _*) => ItemsInBox(leftItems ++ rightItems : _*)
      case EmptyBox => EmptyBox
    }
    case EmptyBox => box
  }

  override def isEmpty: Boolean = this match {
    case ItemsInBox(items @ _*) => items.isEmpty
    case EmptyBox => true
  }

  override def length: Int = this match {
    case ItemsInBox(items @ _*) => items.length
    case EmptyBox => 0
  }

}
case class ItemsInBox[+T <: Object](items : T*) extends Box[T]
case object EmptyBox extends Box[Nothing]

trait Group[+T] extends Collection {
  def ++[S >: T <: Object](group:Group[S]):Group[S] = this match {
    case ItemsInGroup(leftItems @ _*) => group match {
      case ItemsInGroup(rightItems @ _*) => ItemsInGroup(leftItems ++ rightItems : _*)
      case EmptyGroup => EmptyGroup
    }
    case EmptyGroup => group
  }

  override def isEmpty: Boolean = this match {
    case ItemsInGroup(items @ _*) => items.isEmpty
    case EmptyGroup => true
  }

  override def length: Int = this match {
    case ItemsInGroup(items @ _*) => items.length
    case EmptyGroup => 0
  }

}
case class ItemsInGroup[+T<:Object](items : T*) extends Group[T]
case object EmptyGroup extends Group[Nothing]

object Box {

  def apply[T <: Object](items:T*): Box[T] =
    if(items.isEmpty) EmptyBox
    else ItemsInBox(items:_*)

  // Monoid[Box[T]]
  implicit def monoid[T <: Object] = new Monoid[Box[T]]{
    override def op(left: Box[T], right: Box[T]): Box[T] = left ++ right
    override def zero: Box[T] = EmptyBox
  }

  def boxFoldable = new Foldable[Box] {
    override def foldMap[A, B](fa: Box[A])(f: A => B)(implicit mb: Monoid[B]): B =
      fa match {
        case ItemsInBox(items @ _*) => items.foldLeft(mb.zero)((acc, i) => mb.op(f(i), acc))
        case EmptyBox => mb.zero
      }
  }

}

object Group {

  def apply[T <: Object](items:T*): Group[T] =
    if(items.isEmpty) EmptyGroup
    else ItemsInGroup(items:_*)

  def monoid[T <: Object] = new Monoid[Group[T]] {
    override def op(left: Group[T], right: Group[T]): Group[T] = left ++ right
    override def zero: Group[T] = EmptyGroup
  }

}