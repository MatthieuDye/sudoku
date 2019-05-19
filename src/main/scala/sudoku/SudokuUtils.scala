package sudoku

import scala.util.matching.Regex
import scala.io.StdIn._
import sudoku.Cell

object SudokuUtils {

  val numberPattern = new Regex("[0-9]")

  val sudoku = List(0,0,0,0,0,4,0,9,0,8,0,2,9,7,0,0,0,0,9,0,1,2,0,0,3,0,0,0,0,0,0,4,9,1,5,7,0,1,3,0,5,0,9,2,0,5,7,9,1,2,0,0,0,0,0,0,7,0,0,2,6,0,3,0,0,0,0,3,8,2,0,5,0,2,0,5,0,0,0,0,0)
  val sudoku2 = List.fill(81)(0)

  val cellTest = new Cell(1,1)
  val cellTest2 = new Cell(1,1,Some(7),Some(List(1)))

  def printCell(c: Cell) : Unit = {
    print(c)
  }

  def inputAction(): Int = {
    println("What do you want to do ?")
    println("1. Add a proposition value")
    println("2. Remove a proposition value")
    println("3. Add a validation value")
    println("4. remove a validation value")

    val action = readLine("Choose action > ")
    return action.toInt
  }

  def executeAction( cell: Cell, action : Int) : Cell = {
    action match {
      case 1 => {
        val inputValue = inputValue()
        cell.addProposition(inputValue)
      }
      case 2 => {
        println("Selected cell's propositions values : "+cell.propositions())
        println("Type a value to remove")
        cell.removeProposition(inputValue())
      }
    }
  }

  def inputValue() : List[Int] = {
    print("Type a value > ")
    return readInt() :: Nil
  }

  def inputRow(): Int = {
    inputIndice("row")
  }

  def inputColumn() : Int = {
    inputIndice("column")
  }

  def inputIndice(typeInput : String) : Int = {

    val indice = readLine("Give a %s (1 to 9)> ".format(typeInput))

    println("Selected number is %s".format(indice))

    numberPattern.findFirstMatchIn(indice) match {
      case Some(_) => {
        println("Number OK")
        indice.toInt
      }
      case None => {
        println("Should be a number between 1 and 9")
        inputColumn()
      }
    }
  }

  def createCellList(figures : List[Int]) : List[Cell] = {
    val sudokuList = List.tabulate(81)( i => new Cell((i%10)+(i/10),(i/10),Some(figures.apply(i))))

    return sudokuList
  }

  def createGridFromCellList(cells : List[Cell]) : Grid = {
    return new Grid(cells)
  }

  def printGrid(grid: Grid) : Unit = {
    println("-------------")
    for (i<- 1 to 9) {
      for (j<- 1 to 9) {
        if (j%3==1) print("|")
        print(grid.cells().apply(i+j-2).toString)
        if (j==9) print("|")
      }
      println()
      if (i%3==0) {
        println("-------------")
      }

    }
  }
}
