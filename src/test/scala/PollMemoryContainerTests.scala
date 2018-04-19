import java.util.{Calendar, Date}

import container.PollMemoryContainer
import org.scalatest.{FlatSpec, Matchers}
import structures.Poll

class PollMemoryContainerTests extends FlatSpec with Matchers{
  val somePoll = Poll("user", "name")
  val past: Date = Calendar.getInstance().getTime
  past.setYear(past.getYear - 1)
  val future: Date = Calendar.getInstance().getTime
  future.setYear(future.getYear + 1)

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

  it should "update polls properly" in {
    val p = new PollMemoryContainer()
    assert(p.get(p.add(Poll("u", "n", start_time = Some(past)))).get.started)
    assert(!p.get(p.add(Poll("u", "n", start_time = Some(future)))).get.started)
    assert(p.get(p.add(Poll("u", "n", stop_time = Some(past)))).get.stopped)
    assert(!p.get(p.add(Poll("u", "n", stop_time = Some(future)))).get.stopped)
  }

  "start, stop and delete" should "work correctly" in {
    val p = new PollMemoryContainer()
    val id = p.add(somePoll)
    p.start(id)
    assert(p.get(id).get.started)
    p.stop(id)
    assert(p.get(id).get.stopped)
    p.delete(id)
    assert(p.get(id).isEmpty)
  }

  "toList" should "work correctly" in {
    val p = new PollMemoryContainer()
    p.add(somePoll)
    p.add(somePoll)
    assert(p.toList.lengthCompare(2) == 0)
  }
}
