package simulations

import org.scalacheck._

class EpidemySpecification extends Properties("EpidemySimulator") {
  val epidemyGen: Gen[EpidemySimulator] = new EpidemySimulator	
  
  property("1 % population is affected") = forAll {
    (gen: EpidemySimulator) => gen. 
  }
}