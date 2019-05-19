package sudoku

case class Solver(grid: Vector[Vector[AbstractCell]) {

  def isAllowedInRow(position: (Int, Int), value : Int) : Boolean = {
    !grid(position._1).contains(Fixed(value))
  }
  def isAllowedInColumn(position: (Int, Int), value : Int) : Boolean = {
    val t = grid.transpose
    !t(position._2).contains(Fixed(value))
  }
  def isAllowedInSquare(position: (Int, Int), value :Int) : Boolean = {
    val ci = position._1 / 3
    val cj = position._2 / 3
    val squareCells = grid.grouped(3).toList(ci).flatMap {
      row => row.grouped(3).toList(cj)
    }
    !squareCells.contains(Fixed(value))
  }

  def isAllowed( position: (Int,Int), value: Int): Boolean ={
    isAllowedInColumn(position, value) &&
    isAllowedInRow(position, value) &&
    isAllowedInSquare(position, value)
  }

  def filter(): Solver = {
    val filtered: Vector[Vector[AbstractCell]] = grid.zipWithIndex
      .map { case (row, rowIdx) =>
        row
        .zipWithIndex
        .map{
          case ( Undetermined(values), colIdx) =>
            Undetermined(values.filter(v => isAllowed((rowIdx, colIdx),v)))
          case (x, _) => x
        }
        .map {
          case Undetermined(values) if values.size == 1 => Fixed(values.head)
          case x => x
        }
      }
    if (filtered == grid) this
    else Solver(filtered).filter()
  }
}
