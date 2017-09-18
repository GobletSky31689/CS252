abstract class HList[H, T <: HList[_, _]] {
  type Append[L <: HList[_, _]] <: HList[_, _]
  
  def head: H
  def tail: T
  def append[L <: HList[_, _]](lst: L): Append[L]
}

case object HEmpty extends HList[Nothing, Nothing] {
  type Append[L <: HList[_, _]] = L
  
  def head = ???
  def tail = ???
  def ::[A](a: A) = HNonEmpty(a, this)
  def append[L <: HList[_, _]](lst: L) = lst
}

case class HNonEmpty[H, T <: HList[_, _]](head: H, tail: T) extends HList[H, T] {
  type Append[L <: HList[_, _]] = HNonEmpty[H, T#Append[L]]
  
  def ::[A](a: A) = HNonEmpty(a, this)
  
  def append[L <: HList[_, _]](lst: L) = HNonEmpty(head, tail.append(lst))
}

abstract class JsonConverter[T]{
  def convert(t: T): String
}

object prog extends App {
  implicit object stringJsonConverter extends JsonConverter[String]{
    def convert(t: String) = "\"" + t.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
  }
  implicit object doubleJsonConverter extends JsonConverter[Double]{
    def convert(t: Double) = "" + t
  }
  implicit object intJsonConverter extends JsonConverter[Int]{
    def convert(t: Int) = "" + t
  }
  implicit def listJsonConverter[T  : JsonConverter]: JsonConverter[List[T]] = new JsonConverter[List[T]] {
    def convert(ts: List[T]) = ts.map(toJson(_)).mkString("[", ",", "]")
  }
  
  def toJson[T](t: T)(implicit converter: JsonConverter[T]) = converter.convert(t)

  // or def toJson[T : JsonConverter](t: T) = implicitly[JsonConverter[T]].convert(t)

  println(toJson(List(List(3, 4), List(5, 6))))  
  // println(toJson(List(List(3, 4), 5)))
  // Note that this can't work because you can't covert a List[Any]

  def splice(h: String, t: String) = if (t == "[]") "[" + h + "]" else "[" + h + "," + t.substring(1) 


  // Complete these declarations
  implicit object hEmptyJsonConverter extends JsonConverter[HEmpty.type] {
    def convert(t: HEmpty.type) = "[]"
  }
  
  implicit def hNonEmptyJsonConverter[H : JsonConverter, T <: HList[_, _] : JsonConverter ]: JsonConverter[HNonEmpty[H, T]] = new JsonConverter[HNonEmpty[H, T]] {
    def convert(t: HNonEmpty[H, T]) = splice(toJson(t.head), toJson(t.tail))
  }

  // Don't mess with these. Make it so that they print the right thing.
  // println(toJson("Fred" :: HEmpty))
  println(toJson(3 :: "Fred" :: HEmpty))
  println(toJson((3 :: 4 :: HEmpty) :: 5 :: HEmpty))
}
