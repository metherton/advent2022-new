import scala.io.Source

trait Puzzle {
  def run(): Unit
}

object Puzzles {
  val sourceLists = (for {
    i <- 1 to 14
  } yield Source.fromFile(s"/Users/martinetherton/Developer/projects/be/scala/adventofcode2022/advent2022/src/$i.txt").getLines.toList).toList
  val puzzles = List(new Puzzle1(sourceLists(0)),
    new Puzzle2(sourceLists(1)),
    new Puzzle3(sourceLists(2)),
    new Puzzle4(sourceLists(3)),
    new Puzzle5(sourceLists(4)),
    new Puzzle6(sourceLists(5)),
    new Puzzle7(sourceLists(6)),
    new Puzzle8(sourceLists(7)),
    new Puzzle9(sourceLists(8)),
    new Puzzle10(sourceLists(9)),
    new Puzzle11(sourceLists(10)),
    new Puzzle12(sourceLists(11)),
    new Puzzle13(sourceLists(12)),
    new Puzzle14(sourceLists(13))
  )

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

case class Puzzle9(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val Pattern = "move.*".r
    val NumberPattern = " 1.*".r
    val result = l.foldLeft(List[List[String]](List(), List(), List(), List(), List(), List(), List(), List(), List()): List[List[String]])((acc, el) => el match {
      case "" => List(acc(0).reverse, acc(1).reverse, acc(2).reverse, acc(3).reverse, acc(4).reverse, acc(5).reverse, acc(6).reverse, acc(7).reverse, acc(8).reverse)
      case Pattern() => {
        val moves = el.split(" ").toList.filter(e => e.forall(Character.isDigit)).map(_.toInt)
        val letterToMove = acc(moves(1) - 1).take(moves(0)).reverse
        val acc0 = if (moves(1) - 1 == 0) {
          acc(moves(1) - 1).drop(moves(0))
        } else if (moves(2) - 1 == 0) {
          letterToMove ::: acc(0)
        } else {
          acc(0)
        }
        val acc1 = if (moves(1) - 1 == 1) {
          acc(1).drop(moves(0))
        } else if (moves(2) - 1 == 1) {
          letterToMove ::: acc(1)
        } else {
          acc(1)
        }
        val acc2 = if (moves(1) - 1 == 2) {
          acc(2).drop(moves(0))
        } else if (moves(2) - 1 == 2) {
          letterToMove ::: acc(2)
        } else {
          acc(2)
        }
        val acc3 = if (moves(1) - 1 == 3) {
          acc(3).drop(moves(0))
        } else if (moves(2) - 1 == 3) {
          letterToMove ::: acc(3)
        } else {
          acc(3)
        }
        val acc4 = if (moves(1) - 1 == 4) {
          acc(4).drop(moves(0))
        } else if (moves(2) - 1 == 4) {
          letterToMove ::: acc(4)
        } else {
          acc(4)
        }
        val acc5 = if (moves(1) - 1 == 5) {
          acc(5).drop(moves(0))
        } else if (moves(2) - 1 == 5) {
          letterToMove ::: acc(5)
        } else {
          acc(5)
        }
        val acc6 = if (moves(1) - 1 == 6) {
          acc(6).drop(moves(0))
        } else if (moves(2) - 1 == 6) {
          letterToMove ::: acc(6)
        } else {
          acc(6)
        }
        val acc7 = if (moves(1) - 1 == 7) {
          acc(7).drop(moves(0))
        } else if (moves(2) - 1 == 7) {
          letterToMove ::: acc(7)
        } else {
          acc(7)
        }
        val acc8 = if (moves(1) - 1 == 8) {
          acc(8).drop(moves(0))
        } else if (moves(2) - 1 == 8) {
          letterToMove ::: acc(8)
        } else {
          acc(8)
        }

        val newAcc = List(acc0, acc1, acc2, acc3, acc4, acc5, acc6, acc7, acc8)
        newAcc
      }
      case NumberPattern() => acc
      case line => {
        val acc0: List[String] = if (!line.charAt(1).isWhitespace && !line.charAt(1).isDigit ) { line.charAt(1).toString :: acc(0)} else {acc(0)}
        val acc1: List[String] = if (!line.charAt(5).isWhitespace && !line.charAt(5).isDigit ) {line.charAt(5).toString :: acc(1)} else {acc(1)}
        val acc2: List[String] = if (!line.charAt(9).isWhitespace && !line.charAt(9).isDigit ) {line.charAt(9).toString :: acc(2)} else {acc(2)}
        val acc3: List[String] = if (!line.charAt(13).isWhitespace && !line.charAt(13).isDigit ) {line.charAt(13).toString :: acc(3)} else {acc(3)}
        val acc4: List[String] = if (!line.charAt(17).isWhitespace && !line.charAt(17).isDigit ) {line.charAt(17).toString :: acc(4)} else {acc(4)}
        val acc5: List[String] = if (!line.charAt(21).isWhitespace && !line.charAt(21).isDigit ) {line.charAt(21).toString :: acc(5)} else {acc(5)}
        val acc6: List[String] = if (!line.charAt(25).isWhitespace && !line.charAt(25).isDigit ) {line.charAt(25).toString :: acc(6)} else {acc(6)}
        val acc7: List[String] = if (!line.charAt(29).isWhitespace && !line.charAt(29).isDigit ) {line.charAt(29).toString :: acc(7)} else {acc(7)}
        val acc8: List[String] = if (!line.charAt(33).isWhitespace && !line.charAt(33).isDigit ) {line.charAt(33).toString :: acc(8)} else {acc(8)}
        List[List[String]](acc0, acc1, acc2, acc3, acc4, acc5, acc6, acc7, acc8)
      }
    }).map(l => l.head).mkString
    println(s"Result of puzzle 9 is: $result")
  }
}

case class Puzzle10(l: List[String]) extends Puzzle {
  override def run(): Unit = {

    val Pattern = "move.*".r
    val NumberPattern = " 1.*".r
    val result = l.foldLeft(List[List[String]](List(), List(), List(), List(), List(), List(), List(), List(), List()): List[List[String]])((acc, el) => el match {
      case "" => List(acc(0).reverse, acc(1).reverse, acc(2).reverse, acc(3).reverse, acc(4).reverse, acc(5).reverse, acc(6).reverse, acc(7).reverse, acc(8).reverse)
      case Pattern() => {
        val moves = el.split(" ").toList.filter(e => e.forall(Character.isDigit)).map(_.toInt)
        val letterToMove = acc(moves(1) - 1).take(moves(0))//.reverse
        val acc0 = if (moves(1) - 1 == 0) {
          acc(moves(1) - 1).drop(moves(0))
        } else if (moves(2) - 1 == 0) {
          letterToMove ::: acc(0)
        } else {
          acc(0)
        }
        val acc1 = if (moves(1) - 1 == 1) {
          acc(1).drop(moves(0))
        } else if (moves(2) - 1 == 1) {
          letterToMove ::: acc(1)
        } else {
          acc(1)
        }
        val acc2 = if (moves(1) - 1 == 2) {
          acc(2).drop(moves(0))
        } else if (moves(2) - 1 == 2) {
          letterToMove ::: acc(2)
        } else {
          acc(2)
        }
        val acc3 = if (moves(1) - 1 == 3) {
          acc(3).drop(moves(0))
        } else if (moves(2) - 1 == 3) {
          letterToMove ::: acc(3)
        } else {
          acc(3)
        }
        val acc4 = if (moves(1) - 1 == 4) {
          acc(4).drop(moves(0))
        } else if (moves(2) - 1 == 4) {
          letterToMove ::: acc(4)
        } else {
          acc(4)
        }
        val acc5 = if (moves(1) - 1 == 5) {
          acc(5).drop(moves(0))
        } else if (moves(2) - 1 == 5) {
          letterToMove ::: acc(5)
        } else {
          acc(5)
        }
        val acc6 = if (moves(1) - 1 == 6) {
          acc(6).drop(moves(0))
        } else if (moves(2) - 1 == 6) {
          letterToMove ::: acc(6)
        } else {
          acc(6)
        }
        val acc7 = if (moves(1) - 1 == 7) {
          acc(7).drop(moves(0))
        } else if (moves(2) - 1 == 7) {
          letterToMove ::: acc(7)
        } else {
          acc(7)
        }
        val acc8 = if (moves(1) - 1 == 8) {
          acc(8).drop(moves(0))
        } else if (moves(2) - 1 == 8) {
          letterToMove ::: acc(8)
        } else {
          acc(8)
        }

        val newAcc = List(acc0, acc1, acc2, acc3, acc4, acc5, acc6, acc7, acc8)
        newAcc
      }
      case NumberPattern() => acc
      case line => {
        val acc0: List[String] = if (!line.charAt(1).isWhitespace && !line.charAt(1).isDigit) {
          line.charAt(1).toString :: acc(0)
        } else {
          acc(0)
        }
        val acc1: List[String] = if (!line.charAt(5).isWhitespace && !line.charAt(5).isDigit) {
          line.charAt(5).toString :: acc(1)
        } else {
          acc(1)
        }
        val acc2: List[String] = if (!line.charAt(9).isWhitespace && !line.charAt(9).isDigit) {
          line.charAt(9).toString :: acc(2)
        } else {
          acc(2)
        }
        val acc3: List[String] = if (!line.charAt(13).isWhitespace && !line.charAt(13).isDigit) {
          line.charAt(13).toString :: acc(3)
        } else {
          acc(3)
        }
        val acc4: List[String] = if (!line.charAt(17).isWhitespace && !line.charAt(17).isDigit) {
          line.charAt(17).toString :: acc(4)
        } else {
          acc(4)
        }
        val acc5: List[String] = if (!line.charAt(21).isWhitespace && !line.charAt(21).isDigit) {
          line.charAt(21).toString :: acc(5)
        } else {
          acc(5)
        }
        val acc6: List[String] = if (!line.charAt(25).isWhitespace && !line.charAt(25).isDigit) {
          line.charAt(25).toString :: acc(6)
        } else {
          acc(6)
        }
        val acc7: List[String] = if (!line.charAt(29).isWhitespace && !line.charAt(29).isDigit) {
          line.charAt(29).toString :: acc(7)
        } else {
          acc(7)
        }
        val acc8: List[String] = if (!line.charAt(33).isWhitespace && !line.charAt(33).isDigit) {
          line.charAt(33).toString :: acc(8)
        } else {
          acc(8)
        }
        List[List[String]](acc0, acc1, acc2, acc3, acc4, acc5, acc6, acc7, acc8)
      }
    }).map(l => l.head).mkString
    println(s"Result of puzzle 10 is: $result")
  }
}


case class Puzzle11(l: List[String]) extends Puzzle {
  override def run(): Unit = {

    val line: String = l.head
    val result = line.foldLeft((0, 0, 0))((acc, letter) => {
      if (acc._3 != 0 || acc._2 == 0) {
        (acc._1, acc._2 + 1, acc._3)
      //} //else if (acc._2 < 4) {
       // (acc._1, acc._2 + 1, acc._3)
      } else {
        // this is happy flow when we find valid letter
        val sub = line.substring(acc._1, acc._2)
        if (!sub.contains(letter) && acc._2 >= acc._1 + 3) {
          (acc._1, acc._2, acc._2 + 1)
        } else {
          val newFirst = sub.indexOf(letter) + acc._1 + 1
          (newFirst, acc._2 + 1, 0)
        }
      }
    })


    println(s"Result of puzzle 11 is: ${result._3}")
  }
}

case class Puzzle12(l: List[String]) extends Puzzle {
  override def run(): Unit = {

    val line: String = l.head
    val result = line.foldLeft((0, 0, 0))((acc, letter) => {
      if (acc._3 != 0 || acc._2 == 0) {
        (acc._1, acc._2 + 1, acc._3)
        //} //else if (acc._2 < 4) {
        // (acc._1, acc._2 + 1, acc._3)
      } else {
        // this is happy flow when we find valid letter
        val sub = line.substring(acc._1, acc._2)
        if (!sub.contains(letter) && acc._2 >= acc._1 + 13) {
          (acc._1, acc._2, acc._2 + 1)
        } else {
          val newFirst = sub.indexOf(letter) + acc._1 + 1
          (newFirst, acc._2 + 1, 0)
        }
      }
    })


    println(s"Result of puzzle 12 is: ${result._3}")
  }
}


case class Puzzle13(l: List[String]) extends Puzzle {

  trait FileElement
  case class Directory(name: String, fileElements: Set[FileElement]) extends FileElement
  case class PlainFile(name: String, fileSize: Int) extends FileElement
  case class File(name: String, fileType: String, fileSize: Int, children: List[File], parent: File)

  override def run(): Unit = {

    val Root = "\\$ cd /".r
    val Ls = "\\$ ls".r
    val Dir = raw"dir (.*)".r
    val FileMatcher = raw"([0-9].*) (.*)".r
    val Cd = "\\$ cd (.*)".r
    val BackCd = "\\$ cd ..".r

    type FileStructure = Map[List[String], Set[FileElement]]

    val result: (List[String], FileStructure) = l.foldLeft((List.empty[String], Map.empty: FileStructure)) {
      case ((path, content), line) => line match {
        case Cd(dirname) => dirname match {
          case ".." => (path.init, content)
          case _ => (dirname :: path, content)
        }
        case Ls() => (path, content)
        case Dir(dirname) => (path,content + (path -> (content.getOrElse(path,Set.empty[FileElement]) + Directory(dirname, Set.empty))))
        case FileMatcher(fileSize, fileName) => (path,content + (path -> (content.getOrElse(path,Set.empty[FileElement]) + PlainFile(fileName, fileSize.toInt))))
        case _ => (path, content)
      }
    }


    println(s"Result of puzzle 13 is: $result")
  }
}


case class Puzzle14(l: List[String]) extends Puzzle {



  override def run(): Unit = {





    println(s"Result of puzzle 14 is: ")
  }
}