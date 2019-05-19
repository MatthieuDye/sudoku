package sudoku

import SudokuUtils._

case class GameState(initialGrid : Grid, currentGrid: Grid)

object PlaySudoku extends App {

  val SUDOKU_SIZE = 81;

  val sudoku = List(0,0,0,0,0,4,0,9,0,8,0,2,9,7,0,0,0,0,9,0,1,2,0,0,3,0,0,0,0,0,0,4,9,1,5,7,0,1,3,0,5,0,9,2,0,5,7,9,1,2,0,0,0,0,0,0,7,0,0,2,6,0,3,0,0,0,0,3,8,2,0,5,0,2,0,5,0,0,0,0,0)

  val sudokuGrid = createGridFromCellList(createCellList(sudoku))
 printGrid(sudokuGrid)

  // regex pas bonne
  println()

  executeAction(sudokuGrid.getCell(inputColumn(), inputRow()), inputAction())

  // ajouter valid
    // si valid est vide : nvlle value
    // modifier valid
  // supprimer valid

  // ajouter prop
  // enlever prop
  // vider prop
}
