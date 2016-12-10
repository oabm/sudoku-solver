trait SudokuSolver {
  /**
    * Solve the puzzle.
    *
    * @param puzzle Sudoku puzzle to solve. Will be mutated by this method.
    */
  def solve(puzzle: Puzzle): Unit
}

/**
  * The super simple approach.
  * For each cell, check which numbers are already occupied
  * in its row/column/sector. If only one remains, cell complete!
  * Keep iterating until either the puzzle is complete, or
  * all empty cells have been checked and no changes made.
  *
  * This thing is about as stupid as it gets.
  * Very brute force. Such big-O.
  */
object SimpleSolver extends SudokuSolver {
  override def solve(puzzle: Puzzle): Unit = {
    // NOTE(obm) Easy way to keep track of whether we're stuck.
    // If the puzzle still looks like this after we've tried all
    // the cells, then this strategy has run its course.
    val startingState = puzzle.toString

    // NOTE(obm) Fetch all of the currently unsolved cells.
    val unsolvedCells = puzzle.getAllCells.filter(_.isBlank)

    unsolvedCells.foreach(cell => {
      // NOTE(obm) All cells that share either a
      // row, column, or sector are relvant.
      val relevantCells = puzzle.getColumn(cell.col) ++:
          puzzle.getRow(cell.row) ++:
          puzzle.getSector(cell.col / 3, cell.row / 3)

      // NOTE(obm) We only care about the filled-in cells.
      val affectingCells = relevantCells.filter(_.isFilledIn)

      // NOTE(obm) Extract the numbers, and de-dupe
      val takenNumbers: Set[Int] = affectingCells.map(_.value.get).toSet

      val availableNumbers: Set[Int] = 1.to(9).toSet -- takenNumbers

      if (availableNumbers.isEmpty) {
        // NOTE(obm) We have a cell we can't fill in. No good!
        throw new IllegalStateException(s"Puzzle trouble! row=${cell.row} col=${cell.col} puzzle:\n$puzzle")
      } else if (availableNumbers.size == 1) {
        // NOTE(obm) Sharpen that pencil, we found one!
        puzzle.setCell(cell.col, cell.row, new Cell(cell.col, cell.row, Some(availableNumbers.head)))
      }
    })

    // NOTE(obm) If we finished the puzzle, or didn't make
    // any changes, we're done. Otherwise, keep going!
    if (!(puzzle.isSolved || puzzle.toString == startingState)) {
      solve(puzzle)
    }
  }
}