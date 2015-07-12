import simulations.EpidemySimulator

object check {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(85); 
  val esim = new EpidemySimulator;System.out.println("""esim  : simulations.EpidemySimulator = """ + $show(esim ));$skip(11); 
  esim.run}
}
