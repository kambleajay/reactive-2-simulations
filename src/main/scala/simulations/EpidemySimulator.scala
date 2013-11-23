package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val (top, right, bottom, left) = (1, 2, 3, 4)

    // to complete: additional parameters of simulation
  }

  import SimConfig._

  val persons: List[Person] = {
    val allPersons = List.range(1, 301) map (no => new Person(no))
    assert(allPersons.size == 300)
    allPersons(0).infected = true
    allPersons(1).infected = true
    allPersons(2).infected = true
    allPersons
  }

  //beginning of time
  persons foreach {
    p =>
      {
        if (!p.dead) {
          val dayToMakeMove = (randomBelow(5) + 1)
          p.dayToMove = p.tick + dayToMakeMove
          afterDelay(0)(day(p))
        }
      }
  }

  //Day in a life of a person
  def day(p: Person) {
    if (!p.dead) {
      
      if(p.daysInfected == 14) {
        var die = randomBelow(5)
        if(die == 1) p.dead == true
      } else if(p.daysInfected == 16) {
        p.immune = true
      } else if(p.daysInfected == 18) {
        p.immune = false
        p.daysInfected = 0
        p.infected = false
        p.sick = false
      } 
        
      p.fallSickIfInfected
      if (p.tick == p.dayToMove) {
        p.move
      }
      
      p.tick = p.tick + 1
      if(p.infected) p.daysInfected = p.daysInfected + 1
      
      afterDelay(1)(day(p))
    }
  }

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false
    var tick = 0
    var dayToMove = 0
    var timeToGoSick = 6
    var daysInfected = 0

    override def toString = s"($id) Infected=$infected, Sick=$sick, Immune=$immune, Dead=$dead, Tick=$tick, ToSick=$timeToGoSick, ToMove=$dayToMove"

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //
    def move {
      if (!this.dead) {
        val allPossibleRooms = List(validRoom(row - 1, col), validRoom(row, col + 1), validRoom(row + 1, col), validRoom(row, col - 1))
        val roomStatuses = for {
          room <- allPossibleRooms
          status = roomStatus(room)
        } yield (room, status)
        
        val roomToGo = selectRoom(roomStatuses)
        this.row = roomToGo._1.row
        this.col = roomToGo._1.col
        
        if(roomToGo._2.infected > 0) {
          val toss = randomBelow(2)
          toss match {
            case 0 => this.infected = true
            case _ => 
          }
        }
        
        val dayToMakeMove = (randomBelow(5) + 1)
        this.dayToMove = this.tick + dayToMakeMove
      }
    }

    def selectRoom(roomStats: List[(Room, RoomStatus)]): (Room, RoomStatus) = {
      val safeRooms = for {
        (room, status) <- roomStats
        if status.infected == 0 && status.dead == 0
      } yield (room, status)
      safeRooms match {
        case x :: xs => x
        case Nil => {
          val randomRoom = randomBelow(roomStats.size)
          roomStats(randomRoom)
        }
      }
    }

    def validRoom(row: Int, col: Int): Room = {
      (row, col) match {
        case (0, c) => Room(8, c)
        case (9, c) => Room(1, c)
        case (r, 0) => Room(r, 8)
        case (r, 9) => Room(r, 1)
        case (r, c) => Room(r, c)
      }
    }

    def roomStatus(room: Room): RoomStatus = {
      val peopleInThatRoom = persons filter (p => (p.row == room.row && p.col == room.col))
      var (inf, sic, imm, ded) = (0, 0, 0, 0)
      peopleInThatRoom foreach {
        p =>
          {
            if (p.infected) inf = inf + 1
            if (p.sick) sic = sic + 1
            if (p.immune) imm = imm + 1
            if (p.dead) ded = ded + 1
          }
      }
      RoomStatus(inf, sic, imm, ded)
    }

    def fallSickIfInfected {
      if (this.infected && this.timeToGoSick > 0) {
        this.timeToGoSick = this.timeToGoSick - 1
      } else if (this.infected && this.timeToGoSick == 0 && !this.sick) {
        this.sick = true
      }
    }
  }
}

case class Room(row: Int, col: Int)
case class RoomStatus(infected: Int, sick: Int, immune: Int, dead: Int)
