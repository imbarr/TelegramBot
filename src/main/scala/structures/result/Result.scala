package structures.result

import structures.Poll
import structures.Message._

sealed trait Result

object Result {
  implicit def msgToRes(msg: Message): Result = MsgResult(msg)
}

case class ParseFailureResult(msg: String, column: Int) extends Result

case class MsgResult(msg: Message) extends Result

case class PollCreated(pollId: Int) extends Result

case class ViewList(polls: List[(Int, Poll)]) extends Result

case class ViewResult(poll: Poll) extends Result

case class View(poll: Poll) extends Result

case class QuestionAdded(questionId: Int) extends Result
