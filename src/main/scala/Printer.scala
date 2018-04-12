package main.scala

import main.scala.structures.Message._
import main.scala.structures.result._

class Printer {
  def get(result: Option[Result]): String = result match{
    case Some(r) => r match{
      case x: PollCreated => "Poll " + x.pollId + " was created."
      case x: ViewList => x.polls.map(p => p._1 + " " + p._2.name).mkString("\n")
      case x: ViewResult => "Not implemented."
      case x: MsgResult => msgMap.getOrElse(x.msg, "")
      case _ => ""
    }
    case None => "Command not recognized"
  }

  private val msgMap: Map[Message, String] = Map(
    NoRights -> "Error: You have no rights to do this.",
    PollDeleted -> "Poll was deleted.",
    PollStarted -> "Poll was started.",
    PollEnded -> "Poll was ended.",
    AlreadyStarted -> "Error: Poll already started.",
    AlreadyEnded -> "Error: Poll already ended.",
    NotYetEnded -> "Error: Poll has not yet ended.",
    NotYetStarted -> "Error: Poll has not yet started.",
    StartedByTimer -> "Error: Poll can only be started by a timer.",
    EndedByTimer -> "Error: Poll can only be ended by a timer.",
    NotFound -> "Error: Poll not found.",
    IsNotVisible -> "Error: Poll results can not be viewed before it ends."
  )
}

