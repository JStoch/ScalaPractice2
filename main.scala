import java.awt.Color

@main def main(): Unit = {
  
}

//zad.0
def log(prefix: String)(datetime: String)(text: String) = {
  println("[" + prefix + "] " + datetime + " \t " + text)
}

def color (s: String): String = {
  s match
    case "WARN" => Console.YELLOW + "WARN" + Console.RESET
    case "INFO" => Console.BLUE + "INFO" + Console.RESET
    case "DEBUG" => Console.GREEN + "DEBUG" + Console.RESET
    case "CRITICAL" => Console.RED + "CRITICAL" + Console.RESET
}

//zad.1
def map[A](list: List[A], fun: A => A): List[A] = {
  list match
    case Nil => list
    case _ => fun(list.head)::map(list.tail, fun)
}

//zad.2
def filter[A](list: List[A], pred: A => Boolean): List[A] = {
    list match
      case Nil => list
      case _ => pred(list.head) match
        case true => list.head::filter(list.tail, pred)
        case false => filter(list.tail, pred)

}

//zad. 3
def reduce[A](list: List[A], op: (Int, A) => Int, acc: Int): Int = {
  list match
    case Nil => acc
    case _ => reduce(list.tail, op, op(acc, list.head))
}

//zad. 4
def average(list: List[Int]): Float = {
  val op = (a: Int, b: Int) => a+b
  reduce(list, op, 0).toFloat / list.length
}

//zad. 5
def acronymWithCapital(s: String): String = {
  val sList = s.toList
  val pred = (c: Char) => c >= 65 && c <= 90
  filter(sList, pred).mkString("")
}

def acronymWithSpaces(s: String): String = {
  val sList = s.split(' ').toList
  val firstLetter = (s: String) => s.charAt(0).toString
  map(sList, firstLetter).mkString("")
}

//zad. 6
def filteredSquare(list: List[Int]): List[Int] = {
  val op = (a: Int, b: Int) => a + b
  val sum = reduce(list, op, 0)

  val pred = (a: Int) => (a*a*a <= sum)
  val square = (a: Int) => a*a
  map(filter(list, pred), square)
}
