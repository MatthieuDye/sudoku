import org.scalatest._
import sudoku.Cell

class CellTests extends FunSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  describe("Testing cell initialization") {
    describe("We create a new cell and we verify every field is ok") {
      val newCell = new Cell(1, 1)
      it("The row has been initialized") {
        assert(newCell.row() === 1)
      }
      it("The column has been initialized") {
        assert(newCell.column() === 1)
      }
      it("The propositions list is empty") {
        assert(newCell.propositions().isEmpty)
      }
      it("The validation value is empty") {
        assert(newCell.validation()===0)
      }
    }
  }

  describe("Testing cell update") {
    describe("We create a new cell and we add a new proposition") {
      val firstCell = new Cell(1, 1)
      val secondCell = firstCell.addProposition(9)
      it("The new row has been initialized") {
        assert(secondCell.row() === firstCell.row())
      }
      it("The new column has been initialized") {
        assert(secondCell.column() === firstCell.column())
      }
      it("The new propositions list is updated") {
        assert(secondCell.propositions() != firstCell.propositions())
      }
      it("The new validation value is empty") {
        assert(secondCell.validation() === firstCell.validation())
      }
    }
  }

  describe("Testing cell update") {
    describe("We create a new cell and we add some propositions") {
      val firstCell = new Cell(1, 1)
      val newPropToAdd = List(9,3,1,5)
      val secondCell = firstCell.addPropositions(newPropToAdd)
      it("The new row has been initialized") {
        assert(secondCell.row() === firstCell.row())
      }
      it("The new column has been initialized") {
        assert(secondCell.column() === firstCell.column())
      }
      it("The new propositions list is updated") {
        assert(secondCell.propositions() != firstCell.propositions())
      }
      it("The new validation value is empty") {
        assert(secondCell.validation() === firstCell.validation())
      }
    }
  }

  describe("Testing cell update") {
    describe("We create a new cell and we add some propositions BUT") {
      describe ("This time, there are too many propositions") {
        val firstCell = new Cell(1, 1)
        val newPropToAdd = List(9,3,1,5,5,5,5,5,5,5,5,5,5,5,5,5,5)
        val secondCell = firstCell.addPropositions(newPropToAdd)
        it("The new row has been initialized") {
          assert(secondCell.row() === firstCell.row())
        }
        it("The new column has been initialized") {
          assert(secondCell.column() === firstCell.column())
        }
        it("The new propositions list is not updated") {
          assert(secondCell.propositions() === firstCell.propositions())
        }
        it("The new validation value is empty") {
          assert(secondCell.validation() === firstCell.validation())
        }
      }
    }
  }

  describe("Testing propositions cell update") {
    describe("We create a new cell and we add some propositions BUT") {
      describe ("This time, the proposition list is full") {
        val PropsToAdd = Some(List(9,1,5,3,5,3,5,8,8))
        val firstCell = new Cell(1, 1, None, PropsToAdd)
        val newPropToAdd = 5
        val secondCell = firstCell.addProposition(newPropToAdd)
        it("The two cells are the same object") {
          assert(secondCell.equals(firstCell))
        }
      }
    }
  }

  describe("Testing adding a cell validation") {
    describe("We create a new cell and we add a validation") {
      describe ("Let's test it has correctly been implemented") {
        val firstCell = new Cell(1, 1)
        val secondCell = firstCell.addValidation(4)
        it("The two cells are different objects") {
          assert(!secondCell.equals(firstCell))
        }
        it("The new row has been initialized") {
          assert(secondCell.row() === firstCell.row())
        }
        it("The new column has been initialized") {
          assert(secondCell.column() === firstCell.column())
        }
        it("The new propositions list is the same") {
          assert(secondCell.propositions() === firstCell.propositions())
        }
        it("The new validation value is 4") {
          assert(secondCell.validation() === 4)
        }
      }
    }
  }

  describe("Testing updating a cell validation") {
    describe("We replace a validation input") {
      describe ("Let's test it has correctly been implemented") {
        val firstCell = new Cell(1, 1)
        val secondCell = firstCell.addValidation(4)
        val thirdCell = secondCell.addValidation(5)
        it("The new validation value is 5") {
          assert(thirdCell.validation() === 5)
        }
      }
    }
  }

  describe("Testing updating a failed cell validation") {
    describe("We try to add a validation input which failed") {
      describe ("Let's test it has correctly been implemented") {
        val firstCell = new Cell(1, 1)
        val secondCell = firstCell.addValidation(0)
        it("The new cell is the same as the previous one") {
          assert(secondCell.equals(firstCell))
        }
      }
    }
  }

  describe("Testing removing a proposition") {
    describe("We try to remove a proposition from a cell") {
      describe ("Let's test it has correctly been implemented") {
        val firstCell = new Cell(1, 1)
        val secondCell = firstCell.addProposition(9)
        val thirdCell = secondCell.addProposition(5)
        val fourthCell = thirdCell.removeProposition(9)
        it("The new cell has only one value") {
          assert(fourthCell.propositions().size == 1)
        }
        it("And it's 5") {
          assert(fourthCell.propositions().contains(5))

        }
      }
    }
  }
}