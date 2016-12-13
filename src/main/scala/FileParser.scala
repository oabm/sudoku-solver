import java.net.URL

import scala.io.Source

/**
  * Parses a file with 9 lines of 9 characters into a Puzzle.
  * Any characters other than 1-9 (inclusive) are treated as
  * empty cells.
  */
object FileParser {
  def parse(url: URL): Puzzle =
    parse(Source.fromURL(url).getLines().toList)

  def parse(lines: Seq[String]): Puzzle = {
    require(lines.length == 9, s".sdko file must have 9 lines. ${lines.length} present.")
    val puzzle = new Puzzle()

    lines.zipWithIndex.foreach {
      case (line: String, lineNumber: Int) => {
        require(line.length == 9, s"line ${lineNumber + 1} must have 9 characters. ${line.length} present.")
        line.zipWithIndex.foreach {
          case (char: Char, charIndex: Int) => {
            val cell = char match {
              case '1' => new Cell(charIndex, lineNumber, Some(1))
              case '2' => new Cell(charIndex, lineNumber, Some(2))
              case '3' => new Cell(charIndex, lineNumber, Some(3))
              case '4' => new Cell(charIndex, lineNumber, Some(4))
              case '5' => new Cell(charIndex, lineNumber, Some(5))
              case '6' => new Cell(charIndex, lineNumber, Some(6))
              case '7' => new Cell(charIndex, lineNumber, Some(7))
              case '8' => new Cell(charIndex, lineNumber, Some(8))
              case '9' => new Cell(charIndex, lineNumber, Some(9))
              // Any character other than 1-9 is treated as an empty cell.
              case _ => new Cell(charIndex, lineNumber, None)
            }

            puzzle.setCell(charIndex, lineNumber, cell)
          }
        }
      }
    }

    puzzle
  }
}
