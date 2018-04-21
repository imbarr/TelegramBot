import java.util.{Calendar, Date}

import container.PollMemoryContainer
import org.scalatest.{FlatSpec, Matchers}
import structures.Poll

class PollMemoryContainerTests extends FlatSpec with Matchers{
  val now: Date = Calendar.getInstance().getTime
  val somePoll = Poll("user", "name")

  "add and get" should "add and return elements" in {
    val p = new PollMemoryContainer()
    val id = for(i <- 0 to 20) yield p.add(Poll("u", i.toString))
    for(i <- id)
      assert(p.get(i).get.name == i.toString)
  }

  "get" should "not return non-existing elements" in {
    val p = new PollMemoryContainer()
    assert(p.get(p.add(somePoll) + 1).isEmpty)
  }

  "toList" should "work correctly" in {
    val p = new PollMemoryContainer()
    p.add(somePoll)
    p.add(somePoll)
    assert(p.toList.lengthCompare(2) == 0)
  }
}
