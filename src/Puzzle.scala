import scala.io.Source

trait Puzzle {
  def run(): Unit
}

case class Puzzle1(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val newc = l.foldLeft(List[List[String]](List()))((acc, el) => {
      if (el.isEmpty) {
        List() :: acc
      } else {
        acc match {
          case h :: t => (el :: h) :: t
        }
      }
    })
    val result = newc.map(l => l.map(e => e.toInt).sum).max
    println(s"Result of puzzle 1 is: $result")
  }
}

case class Puzzle2(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val newc = l.foldLeft(List[List[String]](List()))((acc, el) => {
      if (el.isEmpty) {
        List() :: acc
      } else {
        acc match {
          case h :: t => (el :: h) :: t
        }
      }
    })
    val result = newc.map(l => l.map(e => e.toInt).sum).sorted.reverse.take(3).sum
    println(s"Result of puzzle 2 is: $result")
  }
}

case class Puzzle3(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val result = l.map(el => el.split(" ")).map(arr => (arr(0), arr(1))).foldLeft(0)((acc, el) => el match {
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
    println(s"Result of puzzle 3 is: $result")
  }
}

case class Puzzle4(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val result = l.map(el => el.split(" ")).map(arr => (arr(0), arr(1))).foldLeft(0)((acc, el) => el match {
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
    println(s"Result of puzzle 4 is: $result")
  }
}

object Puzzles {

  val l1 = Source.fromFile(s"/Users/martinetherton/Developer/projects/be/scala/adventofcode2022/advent2022/src/3.txt").getLines.toList
  val sourceLists = (for {
    i <- 1 to 4
  } yield Source.fromFile(s"/Users/martinetherton/Developer/projects/be/scala/adventofcode2022/advent2022/src/$i.txt").getLines.toList).toList
  val puzzles = List(new Puzzle1(sourceLists(0)),
    new Puzzle2(sourceLists(1)),
    new Puzzle3(sourceLists(2)),
    new Puzzle4(sourceLists(3)))
}