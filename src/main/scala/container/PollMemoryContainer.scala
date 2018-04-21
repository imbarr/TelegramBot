package container

import structures.{Poll, Question}

class PollMemoryContainer extends PollContainer {
  private var polls = Map.empty[Int, Poll]

  override def get(key: Int): Option[Poll] = polls.get(key) match {
    case None => None
    case Some(x) => Some(x.updated())
  }

  override def start(key: Int): Unit = modify(key, _.start())

  override def stop(key: Int): Unit = modify(key, _.end())

  override def addQuestion(key: Int, question: Question): Option[Int] =
    polls.get(key) match {
      case None => None
      case Some(poll) =>
        val pair = poll.add(question)
        polls += (key -> pair._1)
        Some(pair._2)
    }

  private def modify(key: Int, f:Poll => Poll): Unit =
    if(polls.get(key).isDefined)
      polls += (key -> f(polls(key)))

  override def add(value: Poll): Int = {
    val id = polls.keys.max + 1
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


