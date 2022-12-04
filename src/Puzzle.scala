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

case class Puzzle5(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val priorities = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val result = l.foldLeft(0)((acc, el) => {
      val (first, second) = el.splitAt(el.length/2)
      val prio = first.dropWhile(h => !second.contains(h)).head
      acc + priorities.indexOf(prio) + 1
    })
    println(s"Result of puzzle 5 is: $result")
  }
}

case class Puzzle6(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val priorities = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val group = l.grouped(3).toList
    val result = group.foldLeft(0)((acc, el) => {
      val prio = el(0).dropWhile(h => !(el(1).contains(h) && el(2).contains(h))).head
      acc + priorities.indexOf(prio) + 1
    })
    println(s"Result of puzzle 6 is: $result")
  }
}

case class Puzzle7(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val list = l.map(el => el.split(",")).map(arr => (arr(0).split("-").map(_.toInt), arr(1).split("-").map(_.toInt)))

    val result = list.foldLeft(0)((acc, tup) => {
      if ((tup._1(0) >= tup._2(0) && tup._1(1) <= tup._2(1)) ||
        (tup._2(0) >= tup._1(0) && tup._2(1) <= tup._1(1))) {
          acc + 1
      } else {
        acc
      }
    })

    println(s"Result of puzzle 7 is: $result")
  }
}

case class Puzzle8(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val list = l.map(el => el.split(",")).map(arr => (arr(0).split("-").map(_.toInt), arr(1).split("-").map(_.toInt)))

    val result = list.foldLeft(0)((acc, tup) => {
      if (tup._1(0) <= tup._2(1) && tup._1(1) >= tup._2(0)) {
        acc + 1
      } else {
        acc
      }
    })

    println(s"Result of puzzle 8 is: $result")
  }
}
object Puzzles {
  val sourceLists = (for {
    i <- 1 to 8
  } yield Source.fromFile(s"/Users/martinetherton/Developer/projects/be/scala/adventofcode2022/advent2022/src/$i.txt").getLines.toList).toList
  val puzzles = List(new Puzzle1(sourceLists(0)),
    new Puzzle2(sourceLists(1)),
    new Puzzle3(sourceLists(2)),
    new Puzzle4(sourceLists(3)),
    new Puzzle5(sourceLists(4)),
    new Puzzle6(sourceLists(5)),
    new Puzzle7(sourceLists(6)),
    new Puzzle8(sourceLists(7))
  )

}