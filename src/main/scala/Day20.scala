import java.nio.charset.Charset

import scala.util.Try
import scalaz._
import Scalaz._
import better.files.Resource

object Day20 {

  case class V3(x: Int, y: Int, z: Int) {

    def +(that: V3) = V3(x + that.x, y + that.y, z + that.z)
    def magnitude = Math.abs(x) + Math.abs(y) + Math.abs(z)

    override def toString: String = s"$x,$y,$z"

  }

  // Particle is three vectors and an identifier
  case class Particle(id: Int, p: V3, v: V3, a: V3) {

    override def toString: String = s"p=<$p>, v=<$v>, a=<$a>"

    def dist : Int = p.magnitude

    def tick : Particle = {

      val newV = v + a

      Particle(
        id,
        p + newV,
        newV,
        a
      )
    }

  }

  private val parsePattern = """p=<([-]*[0-9]+),([-]*[0-9]+),([-]*[0-9]+)>, v=<([-]*[0-9]+),([-]*[0-9]+),([-]*[0-9]+)>, a=<([-]*[0-9]+),([-]*[0-9]+),([-]*[0-9]+)>""".r

  def parseParticle(s: String) : Option[Particle] = {

    Try {
      val parsePattern(px,py,pz,vx,vy,vz,ax,ay,az) = s
      Particle(0, V3(px.toInt,py.toInt,pz.toInt), V3(vx.toInt,vy.toInt,vz.toInt), V3(ax.toInt,ay.toInt,az.toInt))
    }.toOption
  }

  def parseParticles(s:String) : List[Particle] = {

    val particles = s.lines

    val particleList = particles.flatMap(parseParticle).toList

    // set the ids based on their position in the list

    particleList.zipWithIndex.map{
      case (p, n) => p.copy(id = n)
    }
  }

  def iterate(particles: List[Particle]) : List[Particle] = {
    particles.map(_.tick)
  }

  def main(args: Array[String]): Unit = {

    val parsed = parseParticle("p=<-317,1413,1507>, v=<19,-102,-108>, a=<1,-3,-3>")

    println(s"parsed $parsed")

    val sample = """p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
                   |p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>""".stripMargin

    val sampleParticles = parseParticles(sample)

    val tickTest1 = sampleParticles(0).tick.toString

    // Basic test of tick...
    assert(tickTest1 === "p=<4,0,0>, v=<1,0,0>, a=<-1,0,0>")

    println(s"parsed $sampleParticles")

    (1 to 40).foldLeft(sampleParticles) {
      case (particles, _) =>

        //println(particles)

        val newP = iterate(particles)

        println(newP)

        val nearest: Particle = newP.minBy(p => p.p.magnitude)

        println(s"nearest ${nearest.id} $nearest dist ${nearest.p.magnitude}")

        newP
    }

    val step1Particles = parseParticles(Resource.getAsString("input20.txt")(Charset.forName("US-ASCII")))


    // A lazy way to get the answer is to run it for a while, see which element stabilizes as the nearest and see if it
    // is the right answer. I'm lazy so I did that.

    (1 to 100).foldLeft(step1Particles) {
      case (particles, _) =>

        //println(particles)

        val newP = iterate(particles)

        println(newP)

        val nearest: Particle = newP.minBy(p => p.p.magnitude)

        println(s"nearest ${nearest.id} $nearest dist ${nearest.p.magnitude}")

        newP
    }

    // Step 2

    // This time we will iterate, removing colliding particles and discover the number of particles once the collisions stop

    // Step 2 sample

    val step2Sample = """p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>"""

    val step2SampleParticles = parseParticles(step2Sample)

    // Our lazy way of iterating until the answer stabilizes is good here too,
    // the answer for the sample and the step2 are found by running 1000 steps and
    // entering the value...

    //(1 to 1000).foldLeft(step2SampleParticles) {
    (1 to 1000).foldLeft(step1Particles) {
      case (particles, _) =>

        println(s"Remaining particles ${particles.size}")

        val newP = iterate(particles)

        // Create a map of particle position to list of particle indices
        val collideMap = newP.foldLeft(Map.empty[V3, List[Particle]]) {

          case (acc, p) =>

            val lp = acc.getOrElse(p.p, List.empty[Particle])

            acc updated (p.p, p +: lp)
        }

        // remaining particles are those that occur once only in the map

        collideMap.values.toList.filterNot{_.size > 1}.flatten


    }



  }


}

/*
--- Day 20: Particle Swarm ---
Suddenly, the GPU contacts you, asking for help. Someone has asked it to simulate too many particles, and it won't be
able to finish them all in time to render the next frame at this rate.

It transmits to you a buffer (your puzzle input) listing each particle in order (starting with particle 0, then particle
1, particle 2, and so on). For each particle, it provides the X, Y, and Z coordinates for the particle's position
(p), velocity (v), and acceleration (a), each in the format <X,Y,Z>.

Each tick, all particles are updated simultaneously. A particle's properties are updated in the following order:

  Increase the X velocity by the X acceleration.
Increase the Y velocity by the Y acceleration.
Increase the Z velocity by the Z acceleration.
Increase the X position by the X velocity.
Increase the Y position by the Y velocity.
Increase the Z position by the Z velocity.
Because of seemingly tenuous rationale involving z-buffering, the GPU would like to know which particle will stay
 closest to position <0,0,0> in the long term. Measure this using the Manhattan distance, which in this situation
 is simply the sum of the absolute values of a particle's X, Y, and Z position.

  For example, suppose you are only given two particles, both of which stay entirely on the X-axis (for simplicity).
  Drawing the current states of particles 0 and 1 (in that order) with an adjacent a number line and diagram of current
   X positions (marked in parenthesis), the following would take place:

  p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)

p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)

p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)

p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)
At this point, particle 1 will never be closer to <0,0,0> than particle 0, and so, in the long run, particle 0 will
stay closest.

Which particle will stay closest to position <0,0,0> in the long term?

*/
