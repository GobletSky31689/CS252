// /*
//   Your task is to complete the reverse method.
// */
// abstract class HList[H, T <: HList[_, _]] {
//   type Append[L <: HList[_, _]] <: HList[_, _]
//   type Reverse <: HList[_, _]
//   def head: H
//   def tail: T
//   def append[L <: HList[_, _]](lst: L): Append[L]
//   def reverse: Reverse
// }

// case object HEmpty extends HList[Nothing, Nothing] {
//   type Append[L <: HList[_, _]] = L
//   type Reverse = HEmpty.type
  
//   def head = ???
//   def tail = ???
//   def ::[A](a: A) = HNonEmpty(a, this)
//   def append[L <: HList[_, _]](lst: L) = lst
//   def reverse = HEmpty
// }

// case class HNonEmpty[H, T <: HList[_, _]](head: H, tail: T) extends HList[H, T] {
//   type Append[L <: HList[_, _]] = HNonEmpty[H, T#Append[L]]
//   type Reverse = HNonEmpty.this.tail.Reverse#Append[HNonEmpty[H,HEmpty.type]]
  
//   def ::[A](a: A) = HNonEmpty(a, this)
  
//   def append[L <: HList[_, _]](lst: L) = HNonEmpty(head, tail.append(lst))
//   def reverse = tail.reverse.append(HNonEmpty(head, HEmpty))
// }

// // Don't change anything here.
// object prog extends App {
//   val l1 = 1 :: "Fred" :: HEmpty
//   val l2 = l1.append(l1)
//   println(l1.reverse)
//   println(l2.reverse)
// }
