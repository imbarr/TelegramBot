import structures.Message._
import structures.result._

class Printer {
  def get(result: Result): String = result match{
    case x: PollCreated => "Poll " + x.pollId + " was created."
    case x: QuestionAdded => "Question " + x.questionId + " was created."
    case x: ViewList => x.polls.map(p => p._1 + " " + p._2.name).mkString("\n")
    case _: ViewResult => "Not implemented."
    case _: View => "Not implemented."
    case x: ParseFailureResult => "Parse error at pos " + x.column + ": " + x.msg

    case x: MsgResult => msgMap(x.msg)
  }

  private val msgMap: Map[Message, String] = Map(
    PollDeleted -> "Poll was deleted.",
    PollStarted -> "Poll was started.",
    PollStopped -> "Poll was stopped.",
    Begin -> "Context mode enabled.",
    End -> "Context mode disabled.",
    QuestionDeleted -> "Question was deleted.",
    Answered -> "Answer submitted.",

    NoRights -> "Error: You have no rights to do this.",
    AlreadyStarted -> "Error: Poll already started.",
    AlreadyStopped -> "Error: Poll already ended.",
    NotYetEnded -> "Error: Poll has not yet ended.",
    NotYetStarted -> "Error: Poll has not yet started.",
    StartedByTimer -> "Error: Poll can only be started by a timer.",
    StoppedByTimer -> "Error: Poll can only be ended by a timer.",
    NotFound -> "Error: Poll not found.",
    IsNotVisible -> "Error: Poll results can not be viewed before it ends.",
    AlreadyInContext -> "Error: You are already in context mode.",
    NotInContext -> "Error: You need to be in context mode.",
    NoOptions -> "Error: No options specified.",
    NoOptionsExpected -> "Error: Open question can not have options.",
    QuestionNotFound -> "Error: Question not found.",
    WrongAnswerFormat -> "Error: Wrong answer format.",
    AlreadyAnswered -> "Error: You already answered this question",

    CommandNotFound -> "Error: Command not found."
  )
}

