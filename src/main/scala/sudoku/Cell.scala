package sudoku

class Cell(x: Int, y: Int, default_validation: Option[Int] = None, default_propositions: Option[List[Int]] = None ) {
  private val _column = x
  private val _row = y

  private val _validation = default_validation
  private val _propositions = default_propositions

  def row(): Int = _row
  def column() : Int = _column

  def validation() : Int = _validation.getOrElse(0)
  def propositions() : List[Int] = _propositions.getOrElse(List())

  def addProposition(p: Int): Cell = {
    if (checkPropIsOk(p)) {
      println(p)
      println(this.validation())
      new Cell(this.column(),this.row(), Option(this.validation()), Option(this.propositions() :+ p))
    } else this
  }

  def addPropositions(p: List[Int]): Cell = {
    if (checkPropsAreOk(p)) {
      new Cell( this.column(), this.row(), Option(this.validation()), Option(this.propositions() ++ p))
    } else this
  }

  def checkPropIsOk(p: Int) : Boolean = {
    propositions().size <9 && !propositions().contains(p)
  }

  def checkPropsAreOk(p: List[Int]) : Boolean = {
    !p.isEmpty && propositions().size+p.size<9 && !propositions().contains(p)
  }

  // adding a validation input takes place of the previous one
  def addValidation(p: Int) : Cell = {
    if (checkValIsOk(Some(p))) {
      new Cell(this.column(),this.row(),  Some(p), Option(this.propositions()))
    } else this
  }

  def checkValIsOk(p: Option[Int]) : Boolean = {
    p.isDefined && p.get>0 && p.get<10
  }

  def removeProposition(p: Int) : Cell = {
    if (propositions().contains(p)) {
      new Cell(this.column(),this.row(),  Option(this.validation()), Option(propositions() diff List(p)))
    } else this
  }

  def removeValidation() : Cell = {
    new Cell(this.column(),this.row(),  None, Option(this.propositions()))
  }

  def cleanPropositions() : Cell = {
    new Cell(this.column(), this.row(), Option(this.validation()), None)
  }

  override def toString: String = if (validation()!= 0) { validation().toString } else " "

  def myProps:  String =
    if (propositions().isEmpty) {
    "No propositions for this cell"
  } else propositions().toString()
}
