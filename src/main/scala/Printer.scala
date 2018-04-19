import structures.Message._
import structures.result._

class Printer {
  def get(result: Result): Option[String] = result match{
    case x: PollCreated => Some("Poll " + x.pollId + " was created.")
    case x: ViewList => Some(x.polls.map(p => p._1 + " " + p._2.name).mkString("\n"))
    case x: ViewResult => Some("Not implemented.")
    case x: MsgResult => msgMap.get(x.msg)
  }

  private val msgMap: Map[Message, String] = Map(
    NoRights -> "Error: You have no rights to do this.",
    PollDeleted -> "Poll was deleted.",
    PollStarted -> "Poll was started.",
    PollStopped -> "Poll was stopped.",
    AlreadyStarted -> "Error: Poll already started.",
    AlreadyStopped -> "Error: Poll already ended.",
    NotYetEnded -> "Error: Poll has not yet ended.",
    NotYetStarted -> "Error: Poll has not yet started.",
    StartedByTimer -> "Error: Poll can only be started by a timer.",
    StoppedByTimer -> "Error: Poll can only be ended by a timer.",
    NotFound -> "Error: Poll not found.",
    IsNotVisible -> "Error: Poll results can not be viewed before it ends.",
    NotRecognized -> "Error: Command not recognized."
  )
}

