package simulations

object Test {

  def main(args: Array[String]): Unit = {
    val esim = new EpidemySimulator
    val aPerson = esim.persons.head
    aPerson.probe = true
    
    println(esim.days +" " +aPerson)
    for (i <- 1 to 300) {
      esim.next
      println(esim.days +"> " +aPerson)
    }
  }

}