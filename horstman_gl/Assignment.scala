// /*
//   Your task is to complete the reverse method.
// */
// abstract class GList[+T] {
//   def head: T
//   def tail: GList[T]
//   def isEmpty: Boolean
//   def cons[U >: T](h: U): GList[U] = new GNonEmpty(h, this)
//   def ::[U >: T](h: U): GList[U] = new GNonEmpty(h, this)
//   def append[U >: T](lst: GList[U]): GList[U]
//   def reverse: GList[T]
// }

// case object GEmpty extends GList[Nothing] {
//   override def head = ???
//   override def tail = ???
//   override def isEmpty = true
//   override def append[U](lst: GList[U]) = lst
//   override def reverse = GEmpty
// }

// case class GNonEmpty[T](head: T, tail: GList[T]) extends GList[T] {
//   override def isEmpty = false  
//   override def append[U >: T](lst: GList[U]) = GNonEmpty(head, tail.append(lst))
//   override def reverse = tail.reverse.append(head::GEmpty)
// }

// // Don't change anything here.
// object prog extends App {
//   val l1 = 1 :: 2 :: 3 :: GEmpty
//   val l2 = "A" :: "B" :: "C" :: "D" :: GEmpty
//   println(l1.reverse)
//   println(l2.reverse)
// }
