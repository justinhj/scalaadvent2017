import java.nio.charset.Charset

import better.files.Resource

object Day24 {

  case class Component(first: Int, second: Int) {
    def reverse() = Component(this.second, this.first)
  }

  // Parse the input string into a Set of components. Since the sample and the example data have no repeated
  // components I use Sets in the solution to simplify and speed up the code

  def parseComponents(s: String) : Set[Component] = {

      val strings = s.split('\n')

      strings.map {
        s =>
          val connectors = s.split('/')

          Component(connectors(0).toInt, connectors(1).toInt)

      }.toSet

  }

  // removes a component from the set. checks both ways around
  def removeComponent(comp: Component, components: Set[Component]) : Set[Component] = {

    if(components.contains(comp)) {
      components - comp
    } else {
      components - comp.reverse()
    }

  }

  // The cost of a bridge is the sum of its number of ports
  def cost(components: List[Component]) : Int = {
    components.foldLeft(0) {
      case (acc, comp) =>
        acc + comp.first + comp.second
    }
  }

  // recursively find the strongest bridge
  def getStrongest(currentBridge: List[Component], remainingComponents: Set[Component]) : Int = {

    if(remainingComponents.isEmpty) {
      cost(currentBridge)
    }
    else {
      val portsRequired = currentBridge.head.second

      val validNextComponents = remainingComponents.flatMap {
        comp =>
          if(comp.first == portsRequired) Some(Component(comp.first, comp.second))
          else if(comp.second == portsRequired) Some(Component(comp.second, comp.first))
          else None
      }

      // See what bridges we can make based on the current bridge and the remaining components
      val possibleBridges = validNextComponents.map {
        component =>
          getStrongest(component :: currentBridge, removeComponent(component, remainingComponents))
      }

      if(possibleBridges.isEmpty)
        cost(currentBridge)
      else
        possibleBridges.max
    }
  }

  def main(args: Array[String]): Unit = {

    val sampleComponentsString = """0/2
                             |2/2
                             |2/3
                             |3/4
                             |3/5
                             |0/1
                             |10/1
                             |9/10""".stripMargin


    val sampleComponents = parseComponents(sampleComponentsString)

    val step1ComponentsString = Resource.getAsString("input24.txt")(Charset.forName("US-ASCII"))

    val step1Components = parseComponents(step1ComponentsString)

    println(s"Strongest ${getStrongest(List(Component(0,0)), sampleComponents)}")

    println(s"Strongest ${getStrongest(List(Component(0,0)), step1Components)}")

  }

}


/*
--- Day 24: Electromagnetic Moat ---
The CPU itself is a large, black building surrounded by a bottomless pit. Enormous metal tubes extend outward from the
side of the building at regular intervals and descend down into the void. There's no way to cross, but you need to get inside.

No way, of course, other than building a bridge out of the magnetic components strewn about nearby.

Each component has two ports, one on each end. The ports come in all different types, and only matching types can be
connected. You take an inventory of the components by their port types (your puzzle input). Each port is identified by
 the number of pins it uses; more pins mean a stronger connection for your bridge. A 3/7 component, for example, has a
 type-3 port on one side, and a type-7 port on the other.

Your side of the pit is metallic; a perfect surface to connect a magnetic, zero-pin port. Because of this, the first
port you use must be of type 0. It doesn't matter what type of port you end with; your goal is just to make the bridge
 as strong as possible.

The strength of a bridge is the sum of the port types in each component. For example, if your bridge is made of
components 0/3, 3/7, and 7/4, your bridge has a strength of 0+3 + 3+7 + 7+4 = 24.

For example, suppose you had the following components:

0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10
With them, you could make the following valid bridges:

0/1
0/1--10/1
0/1--10/1--9/10
0/2
0/2--2/3
0/2--2/3--3/4
0/2--2/3--3/5
0/2--2/2
0/2--2/2--2/3
0/2--2/2--2/3--3/4
0/2--2/2--2/3--3/5
(Note how, as shown by 10/1, order of ports within a component doesn't matter. However, you may only use each port on a component once.)

Of these bridges, the strongest one is 0/1--10/1--9/10; it has a strength of 0+1 + 1+10 + 10+9 = 31.

What is the strength of the strongest bridge you can make with the components you have available?

Step 2

The bridge you've built isn't long enough; you can't jump the rest of the way.

In the example above, there are two longest bridges:

0/2--2/2--2/3--3/4
0/2--2/2--2/3--3/5
Of them, the one which uses the 3/5 component is stronger; its strength is 0+2 + 2+2 + 2+3 + 3+5 = 19.

What is the strength of the longest bridge you can make? If you can make multiple bridges of the longest length, pick the strongest one.


 */