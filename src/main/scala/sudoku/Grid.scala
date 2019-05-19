package sudoku

class Grid(values: List[Cell]) {

  // Vector[Vector[Cell]]

  private val _cells = values

  def cells(): List[Cell] = _cells

  def getCell(column:Int, row: Int) : Cell = {
    cells().apply( (column-1) +(row-1)*9)
  }

  def updateGrid(newCell: Cell) : Grid = {
    new Grid(values.updated(cells().indexOf(getCell(newCell.column(), newCell.row())), newCell))
  }
}
