import java.util.{Calendar, Date}

import container.{PollContainer, PollMemoryContainer}
import org.scalatest.{FlatSpec, Matchers}
import structures.Poll

class PollMemoryContainerTests extends FlatSpec with Matchers{
  val somePoll = Poll("user", "name")

  def Tests(container: => PollContainer): Unit = {
    "add and get" should "add and return elements" in {
      val p = container.getClass.newInstance
      val id = for(i <- 0 to 20) yield p.add(Poll("u", i.toString))
      for(i <- id)
        assert(p.get(i).get.name == i.toString)
    }

    "get" should "not return non-existing elements" in {
      val p = container.getClass.newInstance
      assert(p.get(p.add(somePoll) + 1).isEmpty)
    }

    "delete" should "work correctly" in {
      val p = container.getClass.newInstance
      val id = p.add(somePoll)
      assert(p.delete(id + 1).isEmpty)
      assert(p.get(id).contains(somePoll))
      assert(p.delete(id).contains(somePoll))
      assert(p.get(id).isEmpty)
    }

    "set" should "work correctly" in {
      val p = container.getClass.newInstance
      p.set(99, somePoll)
      assert(p.get(99).contains(somePoll))
    }

    "toList" should "work correctly" in {

      val p = container.getClass.newInstance
      p.add(somePoll)
      p.add(somePoll)
      assert(p.toList.lengthCompare(2) == 0)
    }
  }

  it should behave like Tests(new PollMemoryContainer())
}
