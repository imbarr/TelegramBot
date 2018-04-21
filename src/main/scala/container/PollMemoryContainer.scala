package container

import structures.Poll

class PollMemoryContainer extends PollContainer {
  private var polls = Map.empty[Int, Poll]
  private var id = -1

  override def get(key: Int): Option[Poll] = polls.get(key)

  override def set(key: Int, poll: Poll): Unit =
    polls += (key -> poll)

  override def add(value: Poll): Int = {
    id += 1
    polls += (id -> value)
    id
  }

  override def delete(key: Int): Option[Poll] = {
    val p = polls.get(key)
    polls -= key
    p
  }

  override def toList: List[(Int, Poll)] = polls.toList
}


