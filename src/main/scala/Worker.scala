import java.util.{Calendar, Date}

import container.PollContainer
import structures.query._
import structures.Message._
import structures.QuestionType._
import structures._
import structures.result._

import scala.util.parsing.combinator.RegexParsers

class Worker(polls: PollContainer) extends  RegexParsers{
  var currentPoll: Option[Int] = None

  private def now: Date = Calendar.getInstance().getTime

  def processQuery(user: String, query: Option[Query]): Result = query match {
    case Some(q) => q match {
      case x: CreatePollQuery => createPoll(user, x)
      case x: ListQuery => viewList(user, x)
      case x: DeletePollQuery => deletePoll(user, x)
      case x: StartPollQuery => startPoll(user, x)
      case x: StopPollQuery => stopPoll(user, x)
      case x: ResultQuery => viewResult(user, x)
    }
    case None => NotRecognized
  }

  def createPoll(user: String, q: CreatePollQuery): Result = {
    val id = polls.add(Poll(user, q.name, q.isAnon.getOrElse(true),
      q.isVisible.getOrElse(false), q.startTime, q.stopTime))
    PollCreated(id)
  }

  def viewList(user: String, q: ListQuery): Result = ViewList(polls.toList)

  private def wrapContext(q: Query, f: (Int, Poll) => Result): Result = {
    currentPoll match {
      case None => NotInContext
      case Some(id) => polls.get(id) match{
        case None => NotFound
        case Some(poll) => f(id, poll)
      }
    }
  }

  private def wrapNotFound(q: QueryWithId, f: (Int, Poll) => Result): Result =
    polls.get(q.id) match {
      case None => NotFound
      case Some(p) => f(q.id, p)
    }

  private def wrapQuestionNotFound(poll: Poll, q: QueryWithQuestionId, f: (Int, Question) => Result): Result =
    if(poll.questions.lengthCompare(q.id) <= 0) QuestionNotFound
    else f(q.id, poll.questions(q.id))

  private def wrapNoRights(user: String, id: Int, poll: Poll, f: (Int, Poll) => Result): Result = {
    if(poll.user == user) f(id, poll)
    else NoRights
  }

  private def wrapNotFoundAndNoRights(user: String, q: QueryWithId, f: (Int, Poll) => Result) =
    wrapNotFound(q, (id, poll) => wrapNoRights(user, id, poll, f))

  private def wrapContextAndNoRights(user: String, q: Query, f: (Int, Poll) => Result): Result =
    wrapContext(q, (id, poll) => wrapNoRights(user, id, poll, f))

  def deletePoll(user: String, q: DeletePollQuery): Result =
    wrapNotFoundAndNoRights(user, q, (id, _) => {
      polls.delete(id)
      PollDeleted
    })

  def startPoll(user: String, q: StartPollQuery): Result =
    wrapNotFoundAndNoRights(user, q, (id, poll) => {
      if (poll.stopped(now)) AlreadyStopped
      else if (poll.started(now)) AlreadyStarted
      else if (poll.start_time.isDefined) StartedByTimer
      else {
        polls.set(id, poll.start())
        PollStarted
      }
    })

  def stopPoll(user: String, q: StopPollQuery): Result =
    wrapNotFoundAndNoRights(user, q, (id, poll) => {
      if (poll.stopped(now)) AlreadyStopped
      else if (!poll.started(now)) NotYetStarted
      else if (poll.stop_time.isDefined) StoppedByTimer
      else {
        polls.set(id, poll.stop())
        PollStopped
      }
    })

  def viewResult(user: String, q: ResultQuery): Result =
    wrapNotFound(q, (_, poll) => {
      if (!poll.started(now)) NotYetStarted
      else if (!poll.stopped(now) && !poll.isVisible) IsNotVisible
      else ViewResult(poll)
    })

  def begin(user: String, q: BeginQuery): Result =
    if (currentPoll.isDefined) AlreadyInContext
    else wrapNotFound(q, (id, _) => {
      currentPoll = Some(id)
      Begin
    })

  def end(user: String, q: EndQuery): Result =
    if (currentPoll.isEmpty) NotInContext
    else {
      currentPoll = None
      End
    }

  def view(user: String, q: ViewQuery): Result =
    wrapContext(q, (_, poll) => ViewResult(poll))

  def addQuestion(user: String, q: AddQuestionQuery): Result =
    wrapContextAndNoRights(user, q, (id, poll) => q.questionType.getOrElse(Choice) match {
      case Choice =>
        if(q.options.isEmpty) NoOptions
        else addQuestionTo(id, poll, ChoiceQuestion(q.question, q.options))
      case Multiple =>
        if(q.options.isEmpty) NoOptions
        else addQuestionTo(id, poll, MultipleQuestion(q.question, q.options))
      case Open =>
        if(q.options.nonEmpty) NoOptionsExpected
        else addQuestionTo(id, poll, OpenQuestion(q.question))
      })

  private def addQuestionTo(id: Int, poll: Poll, q: Question): QuestionCreated = {
    polls.set(id, poll.add(q))
    QuestionCreated(poll.questions.length + 1)
  }

  def deleteQuestion(user: String, q: DeleteQuestionQuery): Result =
    wrapContextAndNoRights(user, q, (id, poll) =>
      wrapQuestionNotFound(poll, q, (questionId, _) => {
          polls.set(id, poll.delete(questionId))
          QuestionDeleted
      }))

  def choiceAnswer: Parser[Int] = "[0-9]+".r ^^ (_.toInt)

  def multipleAnswer: Parser[List[Int]] = "([0-9] )*[0-9]".r ^^ (s => s.split(" ").map(_.toInt).toList) ^?
    {case x if x.lengthCompare(x.distinct.length) == 0 => x}

  def answerQuestion(user: String, q: AnswerQuestionQuery): Result =
    wrapContext(q, (id, poll) =>
      wrapQuestionNotFound(poll, q, (questionId, question) =>
        if(question.voted.contains(user)) AlreadyAnswered
        else question.addAnswer(user, q.answer, poll.isAnon) match{
          case Some(x) =>
            polls.set(id, poll.set(questionId, x))
            Answered
          case None => WrongAnswerFormat
        }))
}