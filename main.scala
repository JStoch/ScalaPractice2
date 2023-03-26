import java.awt.Color

@main def main(): Unit = {
  
}

//my implementation of functions for operating on lists
def map[A](list: List[A], fun: A => A): List[A] = {
  list match
    case Nil => list
    case _ => fun(list.head)::map(list.tail, fun)
}

def filter[A](list: List[A], pred: A => Boolean): List[A] = {
    list match
      case Nil => list
      case _ => pred(list.head) match
        case true => list.head::filter(list.tail, pred)
        case false => filter(list.tail, pred)

}

def reduce[A](list: List[A], op: (Int, A) => Int, acc: Int): Int = {
  list match
    case Nil => acc
    case _ => reduce(list.tail, op, op(acc, list.head))
}

//calculate the average value
def average(list: List[Int]): Float = {
  val op = (a: Int, b: Int) => a+b
  reduce(list, op, 0).toFloat / list.length
}

//make an acronym from capital letters in the string
def acronymWithCapital(s: String): String = {
  val sList = s.toList
  val pred = (c: Char) => c >= 65 && c <= 90
  filter(sList, pred).mkString("")
}

//make an acronym from first letters of words
def acronymWithSpaces(s: String): String = {
  val sList = s.split(' ').toList
  val firstLetter = (s: String) => s.charAt(0).toString
  map(sList, firstLetter).mkString("")
}

//return a list of squared numbers, which raised to the 3rd power are lower than the sum of all numbers in the list
def filteredSquare(list: List[Int]): List[Int] = {
  val op = (a: Int, b: Int) => a + b
  val sum = reduce(list, op, 0)

  val pred = (a: Int) => (a*a*a <= sum)
  val square = (a: Int) => a*a
  map(filter(list, pred), square)
}
