import java.text.SimpleDateFormat
import java.util.Date
import scala.io.Source

trait Puzzle {
  def run(): Unit

}

object Puzzles {
  val sourceLists = (for {
    i <- 1 to 5
  } yield Source.fromFile(s"/Users/martinetherton/Developer/projects/be/scala/adventofcode2022/advent2022/src/$i.txt").getLines.toList).toList
  val puzzles = List(
    new Puzzle1(sourceLists(0)),
    new Puzzle2(sourceLists(0)),
    new Puzzle3(sourceLists(1)),
    new Puzzle4(sourceLists(1)),
    new Puzzle5(sourceLists(2)),
    new Puzzle6(sourceLists(2)),
    new Puzzle7(sourceLists(3)),
    new Puzzle8(sourceLists(3)),
    new Puzzle9(sourceLists(4)),
  )
}

case class Puzzle1(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val result = l.sliding(2).toList.foldLeft(0)((acc, el) => el match {
      case List(first, second) => if (second > first) acc + 1 else acc
    })
    println(s"Result of puzzle 1 is: ${result}")
  }
}

case class Puzzle2(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val result = l.map(_.toInt).sliding(3).toList.foldLeft((0, Integer.MAX_VALUE))((acc, el) => el match {
      case List(first, second, third) => if ((first + second + third) > acc._2) (acc._1 + 1, first + second + third) else (acc._1, first + second + third)
    })
    println(s"Result of puzzle 2 is: ${result._1}")
  }
}

case class Puzzle3(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val commands = l.map(_.split(" ").toList).map(x => (x(0), x(1).toInt))
    val result = commands.foldLeft((0,0))((acc, el) => el match {
      case ("forward", x) => (acc._1 + x, acc._2)
      case ("down", x) => (acc._1, acc._2 + x)
      case (_, x) => (acc._1, acc._2 - x)
    })
    println(s"Result of puzzle 3 is: ${result._1 * result._2}")
  }
}

case class Puzzle4(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val commands = l.map(_.split(" ").toList).map(x => (x(0), x(1).toInt))
    val result = commands.foldLeft((0,0,0))((acc, el) => el match {
      case ("forward", x) => (acc._1 + x, (acc._2 + (acc._3 * x)), acc._3)
      case ("down", x) => (acc._1, acc._2, acc._3 + x)
      case (_, x) => (acc._1, acc._2, acc._3 - x)
    })
    println(s"Result of puzzle 4 is: ${result._1 * result._2}")
  }
}

case class Puzzle5(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val gammaBinary = l.transpose.map(el => el.foldLeft((0, 0))((acc, i) => {
      if (i == '0') (acc._1 + 1, acc._2) else (acc._1, acc._2 + 1)
    })).map(x => if (x._1 > x._2) "0" else "1")
    val epsilonBinary = gammaBinary.map(c => if (c == "0") "1" else "0")
    println(s"Result of puzzle 5 is: ${Integer.parseInt(gammaBinary.mkString, 2) * Integer.parseInt(epsilonBinary.mkString, 2)}")
  }
}

case class Puzzle6(l: List[String]) extends Puzzle {
  override def run(): Unit = {

    def calc(ls: List[String], i: Int, f: (Int, Int) => Boolean): String = ls match {
      case h :: Nil => h
      case ls => {
        val colToCheck = ls.transpose.toList(i)
        val lsToFilter = ls.filter(el => {
          val charToCheck = if (f(colToCheck.count(_ == '0'), colToCheck.count(_ == '1'))) '0' else '1'
          el.charAt(i) == charToCheck
        })
        calc(lsToFilter, i + 1, f)
      }
    }
    println(s"Result of puzzle 6 is: ${Integer.parseInt(calc(l, 0, (a, b) => a > b), 2) * Integer.parseInt(calc(l, 0, (a, b) => a <= b), 2)}")

  }
}

case class Puzzle7(l: List[String]) extends Puzzle {
  override def run(): Unit = {

    val numberOfPossibleCards = l.size / 5
    val realNumberOfCards = (l.size / 5) + (numberOfPossibleCards / 5)
    var cardList = for {x <- 1 to realNumberOfCards} yield x
    val numberPool = l.head.split(",").map(_.toInt).toList
    val bingoCards: Seq[List[Int]] = l.tail.filter(!_.isEmpty)
      .map(_.trim).map(s => (s.split("[ ]+")
      .toList.map(_.toInt)))

    println(bingoCards)

    def checkForBingo(listToCheck: List[(Int, Int)]): (Int, Int) = {
      val bingoRows = listToCheck.groupBy(_._1).filter(x => x._2.size > 4)
      val bingoColumns = listToCheck.foldLeft(Map[Int, Map[Int, List[Int]]]())((acc, el) => {
        if (!acc.keySet.contains(el._2)) {
          // add column map
          acc + (el._2 -> Map( el._1 / 5 -> List(bingoCards(el._1)(el._2))))
        } else if (!acc(el._2).keySet.contains(el._1 / 5)) {
          // add card map to column
          val colMap: Map[Int, List[Int]] = acc(el._2)
          val newColMap: Map[Int, List[Int]] = colMap + (el._1 / 5 -> List(bingoCards(el._1)(el._2)))
          acc + (el._2 -> newColMap)
        } else {
          // add element to card map list
          val cardList: List[Int] = acc(el._2)(el._1 / 5)
          val newCardList = bingoCards(el._1)(el._2) :: cardList
          val colMap: Map[Int, List[Int]] = acc(el._2)
          val newColMap: Map[Int, List[Int]] = colMap + (el._1 / 5 -> newCardList)
          acc + (el._2 -> newColMap)
        }
      })

      val vals = for {
        cols <- bingoColumns.keySet.toList
        cards <- bingoColumns(cols).keySet.toList
        if bingoColumns(cols)(cards).size > 4

      } yield (cols, cards)

      println("cols")

      val mapped = bingoRows.map(s => List(s._1, s._2))

      if (bingoRows.size > 0) {
        val lastNumber = bingoCards(listToCheck.head._1)(listToCheck.head._2)
        val card = (bingoRows.head._1 / 5) + 1
        val start = (card - 1) * 5
        val end = ((card - 1) * 5) + 4
        val possibleSquares = for {
          rowNum <- start to end
          col <- 0 to 4
        } yield (rowNum, col)
        val emptySquares = possibleSquares.filterNot(tup => listToCheck.contains(tup)).map(x => bingoCards(x._1)(x._2)).sum
        val result = emptySquares * lastNumber

        println("here")
        println(result)
        println(mapped)
        println(mapped.transpose)
        println("there")
        (bingoRows.head._1, 0)
      } else if (vals.size > 0) {
        val lastNumber = bingoCards(listToCheck.head._1)(listToCheck.head._2)
        val card = vals.head._2 + 1
        val start = (card - 1) * 5
        val end = ((card - 1) * 5) + 4
        val possibleSquares = for {
          rowNum <- start to end
          col <- 0 to 4
        } yield (rowNum, col)
        val emptySquares = possibleSquares.filterNot(tup => listToCheck.contains(tup)).map(x => bingoCards(x._1)(x._2)).sum
        val result = emptySquares * lastNumber
        println("here")
        println(mapped)
        println(mapped.transpose)
        println("there")
        (vals.head._1, 0)
      } else (0,0)
    }

    def loop(numbersToChooseFrom: List[Int], acc: List[(Int, Int)]): (Int, Int, List[(Int, Int)]) = numbersToChooseFrom match {
      case h :: t => {
        val chosenNumbers = for {
          i <- 0 to bingoCards.size - 1
          m = bingoCards(i).indexOf(h)
          if m != -1
        } yield (i, m)
        val solution = checkForBingo(chosenNumbers.toList ::: acc)
        if (solution._1 == 0)
          loop(t, chosenNumbers.toList ::: acc)
        else
          (0,0,List())
      }
    }
    val result = loop(numberPool, List())
    println(result)

  }
}

case class Puzzle8(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val numberPool = l.head.split(",").map(_.toInt).toList
    // begin state, numberPool, all the Bingo cards, each square starts with false,
    // on reading the number we process all the cards and set the matching squares to true

    type BingoSquare = (Int, Boolean)
    case class BingoBoard(rowSquares: List[List[BingoSquare]], columnSquares: List[List[BingoSquare]], resolved: (Boolean, String)) {
    }
    case class BingoGame(number: Int, boards: List[BingoBoard]) {
      def run: BingoGame = {

        val newBoards = for {
          board <- boards
          if board.resolved._1 == false
          rowSquares = board.rowSquares.map(s => s.map(d => if (d._1 == number) (d._1, true) else d))
          colSquares = board.columnSquares.map(s => s.map(d => if (d._1 == number) (d._1, true) else d))
          rowsToResolve = rowSquares.map(r => r.filter(s => s._2 == true).size > 4)
          colsToResolve = colSquares.map(r => r.filter(s => s._2 == true).size > 4)
          resolvedRows = rowsToResolve.contains(true)
          resolvedCols = colsToResolve.contains(true)
          resolved = resolvedRows || resolvedCols
          dateResolved = if (resolved) new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSSSS").format(new Date()) else null
        } yield BingoBoard(rowSquares, colSquares, (resolved, dateResolved))
        // we now want to loop over all the boards and check whether any row or column is
        BingoGame(number, newBoards)
      }
    }

    val boards = l.tail.filter(!_.isEmpty)
      .map(_.trim).map(s => (s.split("[ ]+")
      .toList.map(_.toInt)))
    val listBingoBoards = for {
      rowNum <- 0 to boards.size / 5 - 1
      rows = boards.slice(rowNum * 5, (rowNum + 1) * 5)
      rowSquares = rows.map(l => l.map(a => (a, false)))
      columnSquares = rows.transpose.map(d => d.map(x => (x, false)))
    } yield BingoBoard(rowSquares, columnSquares, (false, null))

    def loop(numbers: List[Int], bingoGame: BingoGame): BingoGame = numbers match {
      case List() => {
        println(s"last Game resolved is" + bingoGame.boards.sortBy(d => d.resolved._2).reverse.head)
        bingoGame
      }
      case h :: t if bingoGame.boards.forall(p => p.resolved._1 == true) => {
        val lastGame = bingoGame.boards.sortBy(d => d.resolved._2).reverse.head
        println(s"last Game resolved is" + lastGame)
        val total = lastGame.rowSquares.map(sq => sq.filter(_._2 == false)).flatten.map(_._1).sum * bingoGame.number
        bingoGame
      }
      case h :: t => loop(t, BingoGame(h, bingoGame.boards).run)
    }
    loop(numberPool.tail, BingoGame(numberPool.head, listBingoBoards.toList).run)
println(listBingoBoards)
  }
}

case class Puzzle9(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    //val lines = l.map(_.toInt).toList
    // begin state, numberPool, all the Bingo cards, each square starts with false,
    // on reading the number we process all the cards and set the matching squares to true

    case class Point(x: Int, y: Int)
    case class Line(p1: Point, p2: Point) {
      def intersectingPoints: List[Point] = (p1, p2) match {
        case (Point(x1, y1), Point(x2, y2)) if x1 == x2 =>
          if (y1 < y2) {
            (for {
              y <- y1 to y2
            } yield Point(x1, y)).toList
          } else {
            (for {
              y <- y2 to y1
            } yield Point(x1, y)).toList
          }
        case (Point(x1, y1), Point(x2, y2)) if y1 == y2 =>
          if (x1 < x2) {
            (for {
              x <- x1 to x2
            } yield Point(x, y1)).toList
          } else {
            (for {
              x <- x2 to x1
            } yield Point(x, y1)).toList
          }
        case (Point(x1, y1), Point(x2, y2)) if (y2 - y1).abs == (x2 - x1).abs =>
          if ((x1 < x2) && (y1 < y2)) {
            (for {
              x <- x1 to x2
              y = y1 + x - x1
            } yield Point(x, y)).toList
          } else if ((x1 < x2) && (y2 < y1)) {
            (for {
              x <- x1 to x2
              y = y2 + x2 - x
            } yield Point(x, y)).toList
          } else if ((x2 < x1) && (y1 < y2)) {
            (for {
              x <- x2 to x1
              y = y2 + x2 - x
            } yield Point(x, y)).toList
          } else if ((x2 < x1) && (y2 < y1)) {
            (for {
              x <- x2 to x1
              y = y2 + x - x2
            } yield Point(x, y)).toList
          } else {
            List()
          }
        case _ => List()
      }
    }

    val lines = for {
      xs <- l.map(s => s.split("->").map(c => c.split(",").toList.map(_.trim.toInt)))
      p1 = Point(xs(0)(0), xs(0)(1))
      p2 = Point(xs(1)(0), xs(1)(1))
    } yield Line(p1, p2)

  //  println(lines)

    val points = (for {
      l <- lines
      points = l.intersectingPoints
    } yield points).flatten
    //println(points)
    println(points.groupBy(identity).mapValues(_.size).toMap.filter(s => s._2 >= 2).size)


  }
}