import java.util.{Calendar, Date}

import container.{PollContainer, PollMemoryContainer}
import info.mukel.telegrambot4s.models.User
import org.scalatest.{FlatSpec, Matchers}
import structures.Poll

class PollMemoryContainerTests extends FlatSpec with Matchers{
  val baseUser = User(11, false, "user")
  val basePoll = Poll(baseUser, "name")

  def Tests(container: () => PollContainer): Unit = {
    "add and get" should "add and return elements" in {
      val p = container()
      val id = for(i <- 0 to 20) yield p.add(Poll(baseUser, i.toString))
      for(i <- id)
        assert(p.get(i).get.name == i.toString)
    }

    "get" should "not return non-existing elements" in {
      val p = container()
      assert(p.get(p.add(basePoll) + 1).isEmpty)
    }

    "delete" should "work correctly" in {
      val p = container()
      val id = p.add(basePoll)
      assert(p.delete(id + 1).isEmpty)
      assert(p.get(id).contains(basePoll))
      assert(p.delete(id).contains(basePoll))
      assert(p.get(id).isEmpty)
    }

    "set" should "work correctly" in {
      val p = container()
      p.set(99, basePoll)
      assert(p.get(99).contains(basePoll))
    }

    "toList" should "work correctly" in {
      val p = container()
      p.add(basePoll)
      p.add(basePoll)
      assert(p.toList.lengthCompare(2) == 0)
    }
  }

  it should behave like Tests(() => new PollMemoryContainer())
}
