package simulations

import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Gen.value

class EpidemySpecification extends Properties("EpidemySimulator") {

  val epidemyGen: Gen[EpidemySimulator] = new EpidemySimulator
  
  property("population is 300") = forAll (epidemyGen) { gen: EpidemySimulator => 
    gen.persons.size == 300
  }
  
  property("1% of population is infected") = forAll (epidemyGen) { es: EpidemySimulator =>
    val infectedPeople = es.persons.filter(_.infected == true).size
    val allPeople = es.persons.size
    ((infectedPeople.toDouble) / allPeople) == 0.01
  }
  
  property("a dead person does not move") = forAll (epidemyGen) { es: EpidemySimulator =>
    val deadPerson = es.persons.head
    deadPerson.infected = true
    deadPerson.sick = true
    deadPerson.dead = true
    deadPerson.immune = false
    val (initRow, initCol) = (deadPerson.row, deadPerson.col)
    
    var allWell = true
    val testDays = 100
    while (!es.agenda.isEmpty && es.agenda.head.time < testDays) {
      es.next
      
      allWell = allWell && (deadPerson.row == initRow) && (deadPerson.col == initCol)
      	&& (deadPerson.dead == true)
    }
    allWell
  }
  
  property("a person moves to other room in 5 days") = forAll (epidemyGen) { es: EpidemySimulator =>
    
    println("person moves?")
    val aPerson = es.persons.head
    aPerson.probe = true
    
    var (lastRow, lastCol) = (aPerson.row, aPerson.col)
    val testDays = 6
    var noOfMoves = 0
    while (!es.agenda.isEmpty && es.agenda.head.time < testDays) {
      es.next
      
      if(aPerson.row != lastRow || aPerson.col != lastCol) {
        noOfMoves = noOfMoves + 1
        lastRow = aPerson.row
        lastCol = aPerson.col
      }
    }
    println(s"no of moves=$noOfMoves")
    (noOfMoves > 0 && noOfMoves <= 5)
  }

}

object Checks {
  def main(args: Array[String]) {
    println("starting checks ...")
    val spec = new EpidemySpecification
    spec.check
  }
}