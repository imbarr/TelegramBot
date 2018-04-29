import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import structures.Message._
import structures.Poll
import structures.question.{ChoiceQuestion, MultipleQuestion, OpenQuestion, Question}
import structures.result._

class Printer {
  val calendar: Calendar = Calendar.getInstance()

  def get(result: Result): String = result match{
    case x: PollCreated => "Poll " + x.pollId + " was created."
    case x: QuestionAdded => "Question " + x.questionId + " was created."
    case x: ViewList => viewList(x.polls)
    case x: ViewResult => viewResult(x.poll)
    case x: View => view(x.poll)
    case x: ParseFailureResult => "Parse error at pos " + x.column + ": " + x.msg

    case x: MsgResult => msgMap(x.msg)
  }

  def viewList(polls: List[(Int, Poll)]): String ={
    val list = polls.map(p => p._1 + ": \"" + p._2.name + "\"")
    if(list.nonEmpty) list.mkString("\n") else "Poll list is empty."
  }

  def viewResult(poll: Poll): String =
    s"""Poll "${poll.name}" [${status(poll)}]:
       |${poll.questions.map(questionResult).mkString("\n")}""".stripMargin

  def questionResult(q: Question): String =   s"""  ${q.question}" [${qtype(q)}]:\n""" +
    (q match {
      case x: ChoiceQuestion => choiceQuestionResult(x)
      case x: MultipleQuestion => multipleQuestionResult(x)
      case x: OpenQuestion => openQuestionResult(x)
    })

  def percent(q: ChoiceQuestion, id: Int): Int =
      (q.votes.count(_._2 == id) / q.voted.length.toFloat * 100).toInt

  def percent(q: MultipleQuestion, id: Int): Int =
    (q.votes.count(_._2.contains(id)) / q.voted.length.toFloat * 100).toInt

  def percent(q: OpenQuestion, answer: String): Int =
    (q.votes.count(_._2 == answer) / q.voted.length.toFloat * 100).toInt

  def choiceQuestionResult(q: ChoiceQuestion): String =
    s"""${q.options.zipWithIndex.map(i =>
      "    \"" + i._1 + "\": " + percent(q, i._2 + 1) + "%").mkString("\n")}""".stripMargin

  def multipleQuestionResult(q: MultipleQuestion): String =
    s"""${q.options.zipWithIndex.map(i =>
      "    \"" + i._1 + "\": " + percent(q, i._2) + "%").mkString("\n")}""".stripMargin

  def openQuestionResult(q: OpenQuestion): String =
    s"""${q.votes.map(_._2).toSet[String].map(s =>
      "    \"" + s + "\": " + percent(q, s) + "%").mkString("\n")}""".stripMargin

  def view(poll: Poll): String =
    s"""Poll "${poll.name}" [${status(poll)}]:
       |  Created by: ${poll.user.firstName} (${poll.user.id})
       |  ${if(poll.isAnon) "Anonymous" else "Not anonymous"}
       |  Results can ${if(!poll.isVisible) "not " else ""}be viewed during voting
       |  Start time: ${date(poll.start_time)}
       |  End time: ${date(poll.stop_time)}
       |  Questions:
       |    ${poll.questions.map(q =>
      "\"" + q.question + "\" [" + qtype(q) + "]").mkString("\n    ")}""".stripMargin

  def status(poll: Poll): String = {
    val now = calendar.getTime
    if(poll.stopped(now)) "Ended"
    else if(poll.started(now)) "In progress"
    else "Not started"
  }

  val dateFormat = new SimpleDateFormat("HH:mm:ss yy:MM:dd")
  def date(d: Option[Date]): String =
    d match{
      case Some(x) => dateFormat.format(x)
      case None => "not set"
    }

  def qtype(q: Question): String =
    q match{
      case _: ChoiceQuestion => "Single choice"
      case _: MultipleQuestion => "Multiple choice"
      case _: OpenQuestion => "Open choice"
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
    AlreadyAnswered -> "Error: You already answered this question.",
    OutOfRange -> "Error: Option index is out of range.",
    MustBeDifferent -> "Error: Option indices must be distinct.",

    CommandNotFound -> "Error: Command not found."
  )
}

