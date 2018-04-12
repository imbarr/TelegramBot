package main.scala.structures.result

import main.scala.structures.Poll
import main.scala.structures.Message._

trait Result {}

class MsgResult(val msg: Message) extends Result {}

object Result{
  implicit def msgToRes(msg: Message): Result = new MsgResult(msg)
}

class PollCreated(val pollId: Int) extends Result {}

class ViewList(val polls: List[(Int, Poll)]) extends Result {}

class ViewResult(val poll: Poll) extends Result {}
