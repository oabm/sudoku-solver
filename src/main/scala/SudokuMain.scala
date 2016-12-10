import scala.io.Source

/**
  * Entry point for the program.
  * TODO:
  *  - Arg-parsing to choose input file.
  */
object SudokuMain extends App {
  val lines = Source.fromURL(getClass.getResource("puzzles/01-easy.sdko")).getLines().toList
  var cells =  lines.zipWithIndex.flatMap {
    case (line: String, rowIndex: Int) => line.zipWithIndex.map {
      case (char: Char, colIndex: Int) => char match {
        case '_' => new Cell(colIndex, rowIndex, None)
        case num => new Cell(colIndex, rowIndex, Some(num.asDigit))
      }
    }
  }

  val puzzle = new Puzzle(cells)

  System.out.println(s"$puzzle\n")

  SimpleSolver.solve(puzzle)

  System.out.println(s"$puzzle")
}
