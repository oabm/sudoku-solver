/**
  * A single cell of a Sudoku puzzle.
  *
  * @param col Index of the column in which the cell resides (zero-indexed).
  * @param value The answer for the cell. None if uncertain.
  */
class Cell(val col: Int, val row: Int, val value: Option[Int]) {
  require(0 <= col && col < 9, s"Cell must have col value in range 0-8 (inclusive). $col given.")
  require(0 <= row && row < 9, s"Cell must have row value in range 0-8 (inclusive). $row given.")
  require(value.isEmpty || (0 < value.get && value.get <= 9), s"Cell must have value in range 1-9 (inclusive). $value given.")

  /**
    * Is the cell blank?
    *
    * @return
    */
  def isBlank: Boolean = value.isEmpty

  /**
    * Is the cell filled-in?
    *
    * @return
    */
  def isFilledIn: Boolean = value.isDefined

  /**
    * Returns the value from the input file
    * (digit for filled-in cell, '_' for blank cell).
    *
    * @return
    */
  override def toString = value match {
    case None => "_"
    case Some(num) => num.toString
  }
}
