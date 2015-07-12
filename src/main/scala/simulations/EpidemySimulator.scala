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

  val persons: List[Person] = createPersonsList
  var days = 0

  //beginning of time
  afterDelay(days)(day)

  //Day in a life of a person
  def day {
    for (person <- persons) {
      if (!person.dead) {
        if (person.probe) println("\tmove? $person")
        
        person.fallSickIfInfected
        person.dieIfNeeded
        person.beImmune
        person.beHealthy

        person.incInfected

        if (person.age > 0 && person.age == person.dayToMove) {
          person.move
          person.dayToMove = person.age + (randomBelow(5) + 1)
        } else if (person.age == 0) {
          //set initial day to move
          person.dayToMove = person.age + (randomBelow(5) + 1)
        }
      }
      person.age = person.age + 1
    }
    days = days + 1
    afterDelay(1)(day)
  }

  def createPersonsList = {
    val allPersons = List.range(1, 301) map (no => new Person(no))
    assert(allPersons.size == 300)
    val one = randomBelow(99) + 1
    allPersons(one).setInfected
    val two = randomBelow(99) + 100
    allPersons(two).setInfected
    val three = randomBelow(99) + 200
    allPersons(three).setInfected
    allPersons
  }

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    var age = 0
    var dayToMove = 0
    var timeToGoSick = 0
    var daysInfected = 0

    var probe = false

    override def toString = s"($id) Inf=$infected, Sik=$sick, Imm=$immune, Ded=$dead, Age=$age, ToSick=$timeToGoSick, ToMove=$dayToMove, Room=($row,$col)"

    def setInfected {
      this.infected = true
      this.timeToGoSick = 5
      this.daysInfected = 1
    }

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //
    def move {
      if (!this.dead) {
        val allPossibleRooms = List(left(row, col), top(row, col), right(row, col), bottom(row, col))
        val roomStatuses = for {
          room <- allPossibleRooms
          status = roomStatus(room)
        } yield (room, status)

        val roomToGo = selectRoom(roomStatuses)

        roomToGo match {
          case Some(room) =>
            if(isSafe(room)) {
              this.row = room.row
              this.col = room.col
            }
          case None =>
        }

        infection
      }
    }

    def isSafe(r: Room) = {
      val status = roomStatus(r)
      (status.sick == 0 && status.dead == 0)
    }

    def infection {
      val status = roomStatus(Room(this.row, this.col))
      if (!this.infected) {
        if (status.infected > 0 || status.immune > 0) {
          val infectionProb = randomBelow(100) + 1
          if (infectionProb <= 40 && !this.immune) {
            this.setInfected
          }
        }
      }
    }

    def selectRoom(roomStats: List[(Room, RoomStatus)]): Option[Room] = {
      val safeRooms = for {
        (room, status) <- roomStats
        if status.sick == 0 && status.dead == 0
      } yield (room, status)
      safeRooms match {
        case x :: xs => Some(safeRooms(randomBelow(safeRooms.size))._1)
        case Nil => None
      }
    }

    def left(row: Int, col: Int) = {
      val (newRow, newCol) = (row, col - 1)
      (newRow, newCol) match {
        case (x, -1) => Room(x, 7)
        case (x, y) => Room(x, y)
      }
    }

    def top(row: Int, col: Int) = {
      val (newRow, newCol) = (row - 1, col)
      (newRow, newCol) match {
        case (-1, y) => Room(7, y)
        case (x, y) => Room(x, y)
      }
    }

    def right(row: Int, col: Int) = {
      val (newRow, newCol) = (row, col + 1)
      (newRow, newCol) match {
        case (x, 8) => Room(x, 0)
        case (x, y) => Room(x, y)
      }
    }

    def bottom(row: Int, col: Int) = {
      val (newRow, newCol) = (row + 1, col)
      (newRow, newCol) match {
        case (8, y) => Room(0, y)
        case (x, y) => Room(x, y)
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
      } else if (this.infected && this.timeToGoSick == 0 && !this.sick && !this.immune) {
        this.sick = true
      }
    }

    def dieIfNeeded {
      if (this.infected && this.sick && this.daysInfected == 14) {
        val dieVal = randomBelow(100) + 1
        if(dieVal <= 25) {
          this.dead = true
        }
      }
    }

    def beImmune {
      if(this.infected && this.daysInfected == 16) {
        this.immune = true
        this.sick = false
      }
    }

    def beHealthy {
      if(this.daysInfected == 18) {
        this.infected = false
        this.immune = false
        this.sick = false
      }
    }

    def incInfected {
      if(this.infected) {
        this.daysInfected = this.daysInfected + 1
      }
    }
  }
}

case class Room(row: Int, col: Int)
case class RoomStatus(infected: Int, sick: Int, immune: Int, dead: Int)
