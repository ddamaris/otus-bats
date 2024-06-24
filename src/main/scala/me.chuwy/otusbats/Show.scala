package me.chuwy.otusbats


trait Show[A] {
  def show(a: A): String
}

object Show {

  // 1.1 Instances (`Int`, `String`, `Boolean`)
  implicit val ShowInt: Show[Int] = (i: Int) => i.toString
  implicit val ShowBoolean: Show[Boolean] = (b: Boolean) => b.toString
  implicit val ShowString: Show[String] = (s: String) => s

  // 1.2 Instances with conditional implicit
  implicit def listShow[A](implicit ev: Show[A]): Show[List[A]] = (a: List[A]) => a.map(ev.show).mkString(",")

  // 2. Summoner (apply)
  def apply[T](implicit ev:Show[T]):Show[T] =ev

  // 3. Syntax extensions

  implicit class ShowOps[T](a: T) {
    def show(implicit ev: Show[T]): String = ev.show(a)

    def mkString_[A, B](begin: String, end: String, separator: String)(implicit s: Show[B], ev: A <:< List[B]): String = {
      // with `<:<` evidence `isInstanceOf` is safe!
      val casted: List[B] = a.asInstanceOf[List[B]]
      Show.mkString_(casted, begin, end, separator)
    }
  }

  /** Transform list of `A` into `String` with custom separator, beginning and ending.
   *  For example: "[a, b, c]" from `List("a", "b", "c")`
   *
   *  @param separator. ',' in above example
   *  @param begin. '[' in above example
   *  @param end. ']' in above example
   */
  def mkString_[T: Show](list: List[T], begin: String, end: String, separator: String): String ={
    val ev:Show[T] =Show[T]
    s"$begin${list.map(ev.show).mkString(separator)}$end"
  }

  // 4. Helper constructors

  /** Just use JVM `toString` implementation, available on every object */
  def fromJvm[A]: Show[A] = _.toString
  
  /** Provide a custom function to avoid `new Show { ... }` machinery */
  def fromFunction[A](f: A => String): Show[A] = f(_)
}

object ShowApp extends App{
  import Show._

  val list: List[Int] = List(1,2,3)
  println(list.show)
}
