package sudoku

class Cell(x: Int, y: Int, default_validation: Option[Int] = None, default_propositions: Option[List[Int]] = None ) {
  private val _row = x
  private val _column = y

  private val _validation = default_validation
  private val _propositions = default_propositions

  def row(): Int = _row
  def column() : Int = _column

  def validation() : Int = _validation.getOrElse(0)
  def propositions() : List[Int] = _propositions.getOrElse(List())

  def addProposition(p: Int): Cell = {
    if (checkPropIsOk(p)) {
      new Cell(this.row(), this.column(), Option(this.validation()), Option(this.propositions() :+ p))
    } else this
  }

  def addPropositions(p: List[Int]): Cell = {
    if (checkPropsAreOk(p)) {
      new Cell(this.row(), this.column(), Option(this.validation()), Option(this.propositions() ++ p))
    } else this
  }

  def checkPropIsOk(p: Int) : Boolean = {
    propositions().size <9 && !propositions().contains(p)
  }

  def checkPropsAreOk(p: List[Int]) : Boolean = {
    !p.isEmpty && propositions().size+p.size<9 && !propositions().contains(p)
  }

  // adding a validation input takes place of the previous one
  def addValidation(p: Option[Int]) : Cell = {
    if (checkValIsOk(p)) {
      new Cell(this.row(), this.column(), p, Option(this.propositions()))
    } else this
  }

  def checkValIsOk(p: Option[Int]) : Boolean = {
    p.isDefined
  }

  def removeProposition(p: Int) : Cell = {
    if (propositions().contains(p)) {
      new Cell(this.row(), this.column(), Option(this.validation()), Option(propositions() diff List(p)))
    } else this
  }

  def cleanPropositions() : Cell = {
    new Cell(this.row(), this.column(), Option(this.validation()), None)
  }

  override def toString: String = if (validation()!= 0) { validation().toString } else " "
}
