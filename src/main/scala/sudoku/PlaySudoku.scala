package sudoku

import SudokuUtils._
import scala.annotation.tailrec
import scala.io.StdIn._

case class GameState(initialGrid : Grid, currentGrid: Grid)

object PlaySudoku extends App {

  val sudoku = List(5,0,2,6,0,4,0,9,0,8,0,2,9,7,0,0,0,0,9,0,1,2,0,0,3,0,0,0,0,0,0,4,9,1,5,7,0,1,3,0,5,0,9,2,0,5,7,9,1,2,0,0,0,0,0,0,7,0,0,2,6,0,3,0,0,0,0,3,8,2,0,5,0,2,0,5,0,0,5,2,6)

  val sudokuGrid = createGridFromCellList(createCellList(sudoku))

  play(GameState(sudokuGrid, sudokuGrid))

  @tailrec
  def play(state : GameState) : Unit = {

    printGrid(state.currentGrid)

    println("Do you want to continue ? (y/n) > ")
    readChar() match {
      case 'y' | 'Y' =>
        val updatedCell = executeAction(state.currentGrid.getCell(inputColumn(), inputRow()), inputAction())
        play(GameState(state.initialGrid, state.currentGrid.updateGrid(updatedCell)))
      case 'n' | 'N' =>
        println("Game stopped. Thanks for playing !")
      case _ =>
        println("Action not in charge.")
        play(state)
    }

  }
}
