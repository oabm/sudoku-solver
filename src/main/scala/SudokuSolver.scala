trait SudokuSolver {
  /**
    * Solve the puzzle.
    *
    * @param puzzle Sudoku puzzle to solve. Will be mutated by this method.
    */
  def solve(puzzle: Puzzle): Unit

  /**
    * Helper method for algorithms which iterate until the puzzle is solved.
    * If an iteration is completed that does not change the puzzle,
    * the algorithm halts, to avoid an infinite loop.
    *
    * @param puzzle
    */
  def solveIteratively(puzzle: Puzzle): Unit = {
    // NOTE(obm) Easy way to keep track of whether we're stuck.
    // If the puzzle still looks like this after we've tried all
    // the cells, then this strategy has run its course.
    val startingState = puzzle.toString

    iteration(puzzle)

    // NOTE(obm) If we finished the puzzle, or didn't make
    // any changes, we're done. Otherwise, keep going!
    if (!(puzzle.isSolved || puzzle.toString == startingState)) {
      solve(puzzle)
    }
  }

  /**
    * Perform one iteration of the algorithm.
    *
    * @param puzzle
    */
  def iteration(puzzle: Puzzle): Unit
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
  override def solve(puzzle: Puzzle) = solveIteratively(puzzle)

  override def iteration(puzzle: Puzzle): Unit = {
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
          puzzle.getSector(cell.sector)

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

/**
  * A slightly more nuanced approach than that of `SimpleSolver`.
  * For each region of cells (column, row, or sector), determine
  * which digits it is missing. Using the other regions, figure
  * out which of the empty cells in the region can hold that digit.
  * If there is only one, fill it in!
  *
  * Also pretty darn brute force.
  */
object IntermediateSolver extends SudokuSolver {
  override def solve(puzzle: Puzzle) = solveIteratively(puzzle)


  override def iteration(puzzle: Puzzle): Unit = {
    // NOTE(obm) Columns first
    0.to(8).foreach(colIndex => {
      val (filledInCells, blankCells) = puzzle.getColumn(colIndex).partition(_.isFilledIn)
      val takenNumbers: Set[Int] = filledInCells.map(_.value.get).toSet
      val availableNumbers: Set[Int] = 1.to(9).toSet -- takenNumbers



    })
  }
}