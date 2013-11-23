import simulations.EpidemySimulator

object check {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(83); 
  val es = new EpidemySimulator;System.out.println("""es  : simulations.EpidemySimulator = """ + $show(es ));$skip(60); 
  val infectedPerson = (es.persons.find { _.infected }).get;System.out.println("""infectedPerson  : check.es.Person = """ + $show(infectedPerson ));$skip(9); 
  es.run}
}
