package sudoku

import scala.util.matching.Regex
import scala.io.StdIn._

object SudokuUtils {

  val numberPattern = new Regex("[0-9]")

  def printCell(c: Cell) : Unit = {
    print(c)
  }

  def inputAction(): Int = {

    // transformer en constante
    println("What do you want to do ?")
    println("1. Add a proposition value")
    println("2. Remove a proposition value")
    println("3. See my propositions for this cell")
    println("4. Add a validation value")
    println("5. Remove a validation value")
    println("6. Ask for correction")

    val action = readLine("Choose action > ")

    numberPattern.findFirstMatchIn(action) match {
      case Some(_) =>
        println("Number OK")
        action.toInt
      case None =>
        println("Action not in charge")
        inputAction()
    }
  }

  def executeAction( cell: Cell, action : Int) : Cell = {
    action match {
      case 1 =>
        println("Type a proposition value to add")
        cell.addProposition(inputValue())
      case 2 =>
        println("Selected cell's propositions values : "+cell.propositions())
        println("Type a value to remove")
        cell.removeProposition(inputValue())
      case 4 =>
        println("Type a validation value to add")
        cell.addValidation(inputValue())
      case 5 =>
        println("Selected cell's validation value : "+cell.validation())
        println("Do you really want to remove it ? (y/n) > ")
        if (readChar().equals('y')) {
          println("Cell is removed.")
          cell.removeValidation()
        } else cell
      case 3 =>
        println("Propositions for this cell : ")
        println(cell.myProps)
        cell
      case _ =>
        println("Action not in charge. Nothing happened")
        cell
    }
  }

  def inputValue() : Int = {
    print("Type a value > ")
    readInt()
  }

  def inputRow(): Int = {
    inputIndice("row")
  }

  def inputColumn() : Int = {
    inputIndice("column")
  }

  def inputIndice(typeInput : String) : Int = {

    val indice = readLine("Give a %s (1 to 9) > ".format(typeInput))

    println("Selected number is %s".format(indice))

    numberPattern.findFirstMatchIn(indice) match {
      case Some(_) =>
        println("Number OK")
        indice.toInt
      case None =>
        println("Should be a number between 1 and 9.")
        inputColumn()
    }
  }

  def createCellList(figures : List[Int]) : List[Cell] = {
    val figuresWithIndexes = figures.zipWithIndex

    val sudokuList: List[Cell] = figuresWithIndexes.map( tuple => new Cell( (tuple._2%9)+1, (tuple._2/9)+1, Some(tuple._1) ))

    sudokuList
  }

  def newCell(i: Int ) = new Cell(((i-1)%10)+1,((i-1)/9)+1,Some(i))

  def createGridFromCellList(cells : List[Cell]) : Grid = {
    new Grid(cells)
  }

  def printGrid(grid: Grid) : Unit = {
    println("\n  123 456 789")
    println(" -------------")
    for (y<- 1 to 9) {
      print(y)
      for (x<- 1 to 9) {
        if (x%3==1) print("|")
        print(grid.getCell(x,y))
        if (x==9) print("|")
      }
      println()
      if (y%3==0) {
        println(" -------------")
      }

    }
  }
}
