import scala.io.Source

object Main extends App {

  import Puzzles._
// 2 == 9, 3 == 15 , 4 == 21, 5 === 27, x == (x * 5) + (x - 3) 54 == (54 * 5) + (54 - 3) = 321 323
  puzzles.foreach(test => {
    test.run()
  })
}