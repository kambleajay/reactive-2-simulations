import simulations.EpidemySimulator

object check {
  val es = new EpidemySimulator                   //> es  : simulations.EpidemySimulator = List(WorkItem(0,<function0>), WorkItem(0
                                                  //| ,<function0>), WorkItem(0,<function0>), WorkItem(0,<function0>), WorkItem(0,<
                                                  //| function0>), WorkItem(0,<function0>), WorkItem(0,<function0>), WorkItem(0,<fu
                                                  //| nction0>), WorkItem(0,<function0>), WorkItem(0,<function0>), WorkItem(0,<func
                                                  //| tion0>), WorkItem(0,<function0>), WorkItem(0,<function0>), WorkItem(0,<functi
                                                  //| on0>), WorkItem(0,<function0>), WorkItem(0,<function0>), WorkItem(0,<function
                                                  //| 0>), WorkItem(0,<function0>), WorkItem(0,<function0>), WorkItem(0,<function0>
                                                  //| ), WorkItem(0,<function0>), WorkItem(0,<function0>), WorkItem(0,<function0>),
                                                  //|  WorkItem(0,<function0>), WorkItem(0,<function0>), WorkItem(0,<function0>), W
                                                  //| orkItem(0,<function0>), WorkItem(0,<function0>), WorkItem(0,<function0>), Wor
                                                  //| kItem(0,<function0>), WorkItem(0,<function0>), WorkItem(0,<function0>), WorkI
                                                  //| tem(0,<function0>), WorkItem(0,<function0>), WorkItem(0,<function0>), WorkIte
                                                  //| m(0,<function0>), WorkIte
                                                  //| Output exceeds cutoff limit.
  val infectedPerson = (es.persons.find { _.infected }).get
                                                  //> infectedPerson  : check.es.Person = (1) Infected=true, Sick=false, Immune=fa
                                                  //| lse, Dead=false, Tick=0, ToSick=6, ToMove=1
  es.run                                          //> *** New propagation ***/
}