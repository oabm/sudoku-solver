/**
  * Represents a Sudoku puzzle.
  */
class Puzzle(startingCells: Seq[Cell] = Seq()) {
  val grid: Array[Array[Cell]] = Array.fill(9) { new Array[Cell](9) }
  startingCells.foreach(cell => grid(cell.col)(cell.row) = cell)

  /**
    * Checks whether the puzzle is solved (all cells filled-in).
    *
    * @return
    */
  def isSolved: Boolean =
    !grid.exists(column => column.exists(cell => cell.isBlank))

  /**
    * Getter for an individual cell.
    * Columns are zero-indexed starting from the left.
    * Rows are zero-index starting from the top.
    *
    * @param col
    * @param row
    * @return
    */
  def getCell(col: Int, row: Int): Cell =
    grid(col)(row)

  /**
    * Setter for an individual cell.
    * Columns are zero-indexed starting from the left.
    * Rows are zero-index starting from the top.
    *
    * @param col
    * @param row
    * @param cell
    */
  def setCell(col: Int, row: Int, cell: Cell): Unit =
    grid(col)(row) = cell

  /**
    * Fetch all the cells in the puzzle.
    *
    * @return
    */
  def getAllCells: Array[Cell] = grid.flatten

  /**
    * Fetch the cells of a given column.
    * Columns are zero-index starting from the left.
    *
    * @param col
    * @return
    */
  def getColumn(col: Int): Seq[Cell] = {
    require(0 <= col && col < 9, s"Column must have value 0-8 (inclusive). $col given.")
    0.to(8).map(row => grid(col)(row))
  }

  /**
    * Fetch the cells of a given row.
    * Rows are zero-indexed starting from the top.
    *
    * @param row
    * @return
    */
  def getRow(row: Int): Seq[Cell] = {
    require(0 <= row && row < 9, s"Row must have value 0-8 (inclusive). $row given.")
    0.to(8).map(col => grid(col)(row))
  }

  /**
    * Fetch the cells of a 3x3 sector.
    * Sectors are numbered as follows:
    *    0 1 2 3 4 5 6 7 8
    *   +-----+-----+-----+
    * 0 |     |     |     |
    * 1 |  0  |  1  |  2  |
    * 2 |     |     |     |
    *   +-----+-----+-----+
    * 3 |     |     |     |
    * 4 |  3  |  4  |  5  |
    * 5 |     |     |     |
    *   +-----+-----+-----+
    * 6 |     |     |     |
    * 7 |  6  |  7  |  8  |
    * 8 |     |     |     |
    *   +-----+-----+-----+
    *
    * @param index see above diagram
    * @return
    */
  def getSector(index: Int): Seq[Cell] =
    0.to(2).flatMap(col =>
      0.to(2).map(row =>
        grid((index % 3) * 3 + col)((index / 3) * 3 + row)
      )
    )

  /**
    * Regenerates the orignal String format from
    * which the puzzle was parsed.
    *
    * @return
    */
  override def toString: String =
    0.to(8).map(row =>
      0.to(8).map(col =>
        grid(col)(row)
      ).mkString
    ).mkString("\n")
}

