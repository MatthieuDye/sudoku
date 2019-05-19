package sudoku
import sudoku.Cell

class Grid(values: List[Cell]) {

  private val _cells = values

  def cells(): List[Cell] = _cells

  def getCell(x:Int, y: Int) : Cell = {
    return values.apply(x+y-2)
  }
}
