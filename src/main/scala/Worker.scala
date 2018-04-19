import container.PollContainer
import structures.query._
import structures.Message._
import structures.Poll
import structures.result._

class Worker(polls: PollContainer){
  def processQuery(user: String, query: Option[Query]): Result = query match {
    case Some(q) => q match {
      case x: CreatePollQuery => createPoll(user, x)
      case x: ViewListQuery => viewList(user, x)
      case x: DeletePollQuery => deletePoll(user, x)
      case x: StartPollQuery => startPoll(user, x)
      case x: StopPollQuery => stopPoll(user, x)
      case x: ViewResultQuery => viewResult(user, x)
    }
    case None => NotRecognized
  }

  def createPoll(user: String, q: CreatePollQuery): Result = {
    val id = polls.add(Poll(user, q.name, q.isAnon.getOrElse(true),
      q.isVisible.getOrElse(false), q.startTime, q.stopTime))
    PollCreated(id)
  }

  def viewList(user: String, q: ViewListQuery): Result = ViewList(polls.toList)

  private def wrapPollFunc(user: String, q: QueryWithId, f: Int => Result): Result =
    polls.get(q.id) match {
      case None => NotFound
      case Some(p) =>
        if (p.user == user) f(q.id)
        else NoRights
    }

  def deletePoll(user: String, q: DeletePollQuery): Result =
    wrapPollFunc(user, q, id => {
      polls.delete(id)
      PollDeleted
    })

  def startPoll(user: String, q: StartPollQuery): Result =
    wrapPollFunc(user, q, id => {
      val p = polls.get(id).get
      if (p.stopped) AlreadyStopped
      else if (p.started) AlreadyStarted
      else if (p.start_time.isDefined) StartedByTimer
      else {
        polls.start(id)
        PollStarted
      }
    })

  def stopPoll(user: String, q: StopPollQuery): Result =
    wrapPollFunc(user, q, id => {
      val p = polls.get(id).get
      if (p.stopped) AlreadyStopped
      else if (!p.started) NotYetStarted
      else if (p.stop_time.isDefined) StoppedByTimer
      else {
        polls.stop(id)
        PollStopped
      }
    })

  def viewResult(user: String, q: ViewResultQuery): Result = {
    polls.get(q.id) match {
      case None => NotFound
      case Some(p) =>
        if(!p.started) NotYetStarted
        else if(!p.stopped && !p.isVisible) IsNotVisible
        else ViewResult(p)
    }
  }
}

