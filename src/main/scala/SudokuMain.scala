import java.net.URL

/**
  * Entry point for the program.
  * TODO:
  *  - Add proper arg-parsing
  */
object SudokuMain extends App {
  override def main(args: Array[String]) = {
    val puzzleUrl: URL = getClass.getResource(args(0))

    val puzzle = FileParser.parse(puzzleUrl)

    println(s"$puzzle\n")

    SimpleSolver.solve(puzzle)

    println(puzzle)
  }
}
