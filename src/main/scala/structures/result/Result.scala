package structures.result

import structures.Poll
import structures.Message._

trait Result {}

object Result {
  implicit def msgToRes(msg: Message): Result = MsgResult(msg)
}

case class MsgResult(msg: Message) extends Result {}

case class PollCreated(pollId: Int) extends Result {}

case class ViewList(polls: List[(Int, Poll)]) extends Result {}

case class ViewResult(poll: Poll) extends Result {}
