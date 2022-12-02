import scala.collection.immutable.::
import scala.io.Source

object Main {

  val source = Source.fromFile(s"/Users/martinetherton/Developer/projects/be/scala/adventofcode2022/advent2022/src/input.txt").getLines.toList

  def main(args: Array[String]): Unit = {
    val result = puz4(source)
    println(result)
  }

  def puz4(l: List[String]) = {
    l.map(el => el.split(" ")).map(arr => (arr(0), arr(1))).foldLeft(0)((acc, el) => el match {
      case ("A", "X") => acc + 3
      case ("A", "Y") => acc + 3 + 1
      case ("A", "Z") => acc + 6 + 2 //x
      case ("B", "X") => acc + 1
      case ("B", "Y") => acc + 3 + 2
      case ("B", "Z") => acc + 6 + 3 //x
      case ("C", "X") => acc + 2 //x
      case ("C", "Y") => acc + 3 + 3
      case ("C", "Z") => acc + 6 + 1
    })
  }

  def puz3(l: List[String]) = {
    l.map( el => el.split(" ")).map(arr => (arr(0), arr(1))).foldLeft(0)((acc, el) => el match {
      case ("A", "X") => acc + 3 + 1
      case ("A", "Y") => acc + 6 + 2
      case ("A", "Z") => acc + 3
      case ("B", "X") => acc + 1
      case ("B", "Y") => acc + 3 + 2
      case ("B", "Z") => acc + 6 + 3
      case ("C", "X") => acc + 6 + 1
      case ("C", "Y") => acc + 2
      case ("C", "Z") => acc + 3 + 3
    })
  }

  def puz3(args: Array[String]): Unit = {
    val filename = "input.txt"
    val choiceList = Source.fromFile(s"/Users/martinetherton/Developer/projects/be/scala/adventofcode2022/advent2022/src/$filename")
      .getLines.toList.map(el => el.split(" ")).map(arr => (arr(0), arr(1)))

    val result = choiceList.foldLeft(0)((acc, el) => el match {
      case ("A", "X") => acc + 3 + 1
      case ("A", "Y") => acc + 6 + 2
      case ("A", "Z") => acc + 3
      case ("B", "X") => acc + 1
      case ("B", "Y") => acc + 3 + 2
      case ("B", "Z") => acc + 6 + 3
      case ("C", "X") => acc + 6 + 1
      case ("C", "Y") => acc + 2
      case ("C", "Z") => acc + 3 + 3
    })
    println(result)
  }

  def puz2(args: Array[String]): Unit = {
    val filename = "puz1.txt"
    val calList = Source.fromFile(s"/Users/martinetherton/Developer/projects/be/scala/adventofcode2022/advent2022/src/$filename").getLines.toList
    val newc = calList.foldLeft(List[List[String]](List()))((acc, el) => {
      if (el.isEmpty) {
        List() :: acc
      } else {
        acc match {
          case h :: t => (el :: h) :: t
        }
      }
    })
    println(newc.map(l => l.map(e => e.toInt).sum).sorted.reverse.take(3).sum)
  }

  def puz1(args: Array[String]): Unit = {
    val filename = "puz1.txt"
    val calList = Source.fromFile(s"/Users/martinetherton/Developer/projects/be/scala/adventofcode2022/advent2022/src/$filename").getLines.toList
    val newc = calList.foldLeft(List[List[String]](List()))((acc, el) => {
      if (el.isEmpty) {
        List() :: acc
      } else {
        acc match {
          case h :: t => (el :: h) :: t
        }
      }
    })
    println(newc.map(l => l.map(e => e.toInt).sum).max)
  }
}