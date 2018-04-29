import java.util.{Calendar, Date}

import container.PollContainer
import info.mukel.telegrambot4s.models.User
import structures.query._
import structures.Message._
import structures.QuestionType._
import structures._
import structures.question.{ChoiceQuestion, MultipleQuestion, OpenQuestion, Question}
import structures.result._

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

class Worker(polls: PollContainer, var currentPoll: Map[User, Int] = Map()) extends  RegexParsers{
  private def now: Date = Calendar.getInstance().getTime

  def processQuery(user: User, query: Query): Result = query match {
    case x: CreatePollQuery => createPoll(user, x)
    case x: ListQuery => viewList(user, x)
    case x: DeletePollQuery => deletePoll(user, x)
    case x: StartPollQuery => startPoll(user, x)
    case x: StopPollQuery => stopPoll(user, x)
    case x: ViewResultQuery => viewResult(user, x)
    case x: ViewQuery => view(user, x)
    case x: EndQuery => end(user, x)
    case x: BeginQuery => begin(user, x)
    case x: AddQuestionQuery => addQuestion(user, x)
    case x: DeleteQuestionQuery => deleteQuestion(user, x)
    case x: AnswerQuestionQuery => answerQuestion(user, x)
  }

  def createPoll(user: User, q: CreatePollQuery): Result = {
    val id = polls.add(Poll(user, q.name, q.isAnon.getOrElse(true),
      q.isVisible.getOrElse(false), q.startTime, q.stopTime))
    PollCreated(id)
  }

  def viewList(user: User, q: ListQuery): Result = ViewList(polls.toList)

  private def wrapContext(user: User, q: Query, f: (Int, Poll) => Result): Result = {
    currentPoll.get(user) match {
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

  private def wrapNoRights(user: User, id: Int, poll: Poll, f: (Int, Poll) => Result): Result = {
    if(poll.user == user) f(id, poll)
    else NoRights
  }

  private def wrapNotFoundAndNoRights(user: User, q: QueryWithId, f: (Int, Poll) => Result) =
    wrapNotFound(q, (id, poll) => wrapNoRights(user, id, poll, f))

  private def wrapContextAndNoRights(user: User, q: Query, f: (Int, Poll) => Result): Result =
    wrapContext(user, q, (id, poll) => wrapNoRights(user, id, poll, f))

  def deletePoll(user: User, q: DeletePollQuery): Result =
    wrapNotFoundAndNoRights(user, q, (id, _) => {
      polls.delete(id)
      PollDeleted
    })

  def startPoll(user: User, q: StartPollQuery): Result =
    wrapNotFoundAndNoRights(user, q, (id, poll) => {
      if (poll.stopped(now)) AlreadyStopped
      else if (poll.started(now)) AlreadyStarted
      else if (poll.start_time.isDefined) StartedByTimer
      else {
        polls.set(id, poll.start())
        PollStarted
      }
    })

  def stopPoll(user: User, q: StopPollQuery): Result =
    wrapNotFoundAndNoRights(user, q, (id, poll) => {
      if (poll.stopped(now)) AlreadyStopped
      else if (!poll.started(now)) NotYetStarted
      else if (poll.stop_time.isDefined) StoppedByTimer
      else {
        polls.set(id, poll.stop())
        PollStopped
      }
    })

  def viewResult(user: User, q: ViewResultQuery): Result =
    wrapNotFound(q, (_, poll) => {
      if (!poll.started(now)) NotYetStarted
      else if (!poll.stopped(now) && !poll.isVisible) IsNotVisible
      else ViewResult(poll)
    })

  def begin(user: User, q: BeginQuery): Result =
    if (currentPoll.get(user).isDefined) AlreadyInContext
    else wrapNotFound(q, (id, _) => {
      currentPoll += (user -> id)
      Begin
    })

  def end(user: User, q: EndQuery): Result =
    if (currentPoll.get(user).isEmpty) NotInContext
    else {
      currentPoll -= user
      End
    }

  def view(user: User, q: ViewQuery): Result =
    wrapContext(user, q, (_, poll) => View(poll))

  def addQuestion(user: User, q: AddQuestionQuery): Result =
    wrapContextAndNoRights(user, q, (id, poll) =>
      if(poll.stopped(now)) AlreadyStopped
      else if(poll.started(now)) AlreadyStarted
      else q.questionType.getOrElse(Choice) match {
        case Choice =>
          if(q.options.isEmpty) NoOptions
          else addQuestionTo(id, poll, ChoiceQuestion(q.question, q.options, poll.isAnon))
        case Multiple =>
          if(q.options.isEmpty) NoOptions
          else addQuestionTo(id, poll, MultipleQuestion(q.question, q.options, poll.isAnon))
        case Open =>
          if(q.options.nonEmpty) NoOptionsExpected
          else addQuestionTo(id, poll, OpenQuestion(q.question, poll.isAnon))
        })

  private def addQuestionTo(id: Int, poll: Poll, q: Question): QuestionAdded = {
    polls.set(id, poll.add(q))
    QuestionAdded(poll.questions.length)
  }

  def deleteQuestion(user: User, q: DeleteQuestionQuery): Result =
    wrapContextAndNoRights(user, q, (id, poll) =>
      wrapQuestionNotFound(poll, q, (questionId, _) =>
        if(poll.stopped(now)) AlreadyStopped
        else if(poll.started(now)) AlreadyStarted
        else{
          polls.set(id, poll.delete(questionId))
          QuestionDeleted
      }))

  def answerQuestion(user: User, q: AnswerQuestionQuery): Result =
    wrapContext(user,  q, (id, poll) =>
      wrapQuestionNotFound(poll, q, (questionId, question) =>
        if(poll.stopped(now)) AlreadyStopped
        else if(!poll.started(now)) NotYetStarted
        else if(question.voted.contains(user)) AlreadyAnswered
        else{
          def setter(newQuestion: Question): Unit = polls.set(id, poll.set(questionId, newQuestion))
          question match{
          case x: ChoiceQuestion => answerChoiceQuestion(user, q.answer, x, setter)
          case x: MultipleQuestion => answerMultipleQuestion(user, q.answer, x, setter)
          case x: OpenQuestion =>
              setter(x.answer(user, q.answer))
              Answered
        }}))

  private def answerChoiceQuestion(user: User, answer: String, q: ChoiceQuestion,
                                   f: ChoiceQuestion => Unit): Result =
    Try(answer.toInt - 1).toOption match{
      case Some(x) =>
        if(q.options.isDefinedAt(x)){
          f(q.answer(user, x))
          Answered
        }
        else OutOfRange
      case None => WrongAnswerFormat
    }

  private def answerMultipleQuestion(user: User, answer: String, q: MultipleQuestion,
                                     f: MultipleQuestion => Unit): Result =
    Try(answer.split(' ').map(_.toInt - 1)).toOption match{
      case Some(list) =>
        if(list.distinct.lengthCompare(list.length) != 0) MustBeDifferent
        else if(!list.forall(q.options.isDefinedAt)) OutOfRange
        else{
          f(q.answer(user, list.toList))
          Answered
        }
      case None => WrongAnswerFormat
    }
}