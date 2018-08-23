import domain._

object TestMain {
  def main(args: Array[String]): Unit = {

    val boxB = ItemsInBox(Node("B"), Node("B"))
    val boxW = ItemsInBox(Node("W"), Node("W"))

    val boxE = EmptyBox

    val boxBox = Box(
                      Box(
                        Node("B1"),Node("B2")
                      ),
                      Box(
                        Node("W1"),Node("W2")
                      )
                  )

    val boxBoxBox = Box(
                        Box(
                            Box(Node("B3")),
                            Box(Node("B4"))
                        ),
                        Box(
                            Box(Node("W3")),
                            Box(Node("W4")),
                            Box()
                        )
                    )

    val sumBox = Box.monoid.op(boxBox, boxBoxBox)

    println(sumBox)
//
//    import domain.Box._
//    println(fold(Box(Box(), Box(Node("A"), Node("B")), Box(Box(Node("C")), Box(), Node("D")))))
//

  }
}
