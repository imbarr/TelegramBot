package container

import structures.Poll

class PollMemoryContainer extends PollContainer {
  private var polls = Map.empty[Int, Poll]
  private var id = -1

  override def get(key: Int): Option[Poll] = polls.get(key) match {
    case None => None
    case Some(x) => Some(x.updated())
  }

  override def start(key: Int): Unit = modify(key, p => p.start())

  override def stop(key: Int): Unit = modify(key, p => p.end())

  private def modify(key: Int, f:Poll => Poll): Unit =
    if(polls.get(key).isDefined)
      polls += (key -> f(polls(key)))

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


