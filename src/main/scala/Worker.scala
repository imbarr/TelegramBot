import container.PollContainer
import structures.query._
import structures.Message._
import structures.QuestionType._
import structures.{Poll, Question}
import structures.result._

class Worker(polls: PollContainer){
  var currentPoll: Option[Int] = None

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

  private def wrapNoRights(user: String, id: Int, poll: Poll, f: (Int, Poll) => Result): Result = {
    if(poll.user == user) f(id, poll)
    else NoRights
  }

  private def wrapNotFoundAndNoRights(user: String, q: QueryWithId, f: (Int, Poll) => Result) =
    wrapNotFound(q, (id, poll) => wrapNoRights(user, id, poll, f))

  private def wrapContextAndNoRights(user: String, q: Query, f: (Int, Poll) => Result): Result =
    wrapContext(q, (id, poll) => wrapNoRights(user, id, poll, f))

  def deletePoll(user: String, q: DeletePollQuery): Result =
    wrapNotFoundAndNoRights(user, q, (id, poll) => {
      polls.delete(id)
      PollDeleted
    })

  def startPoll(user: String, q: StartPollQuery): Result =
    wrapNotFoundAndNoRights(user, q, (id, poll) => {
      if (poll.stopped) AlreadyStopped
      else if (poll.started) AlreadyStarted
      else if (poll.start_time.isDefined) StartedByTimer
      else {
        polls.start(id)
        PollStarted
      }
    })

  def stopPoll(user: String, q: StopPollQuery): Result =
    wrapNotFoundAndNoRights(user, q, (id, poll) => {
      if (poll.stopped) AlreadyStopped
      else if (!poll.started) NotYetStarted
      else if (poll.stop_time.isDefined) StoppedByTimer
      else {
        polls.stop(id)
        PollStopped
      }
    })

  def viewResult(user: String, q: ResultQuery): Result =
    wrapNotFound(q, (id, poll) => {
      if (!poll.started) NotYetStarted
      else if (!poll.stopped && !poll.isVisible) IsNotVisible
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
    wrapContextAndNoRights(user, q, (id, _) => {
        val qtype = q.questionType.getOrElse(Choice)
        if((qtype == Choice || qtype == Multiple) && q.options.isEmpty) NoOptions
        else if (qtype == Open && q.options.nonEmpty) NoOptionsExpected
        else QuestionCreated(polls.addQuestion(id, Question(q.question, q.options, Choice)).get)
      })

  def deleteQuestion(user: String, q: DeleteQuestionQuery): Result =
    wrapContextAndNoRights(user, q, (id, _) => {
      if()
    })
}

