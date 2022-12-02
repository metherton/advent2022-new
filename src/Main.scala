import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    import Puzzles._
    puzzles.foreach(test => {
      test.run()
    })
  }

}