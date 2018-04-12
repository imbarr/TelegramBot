package main.scala

import main.scala.container.PollContainer
import main.scala.structures.query._
import main.scala.structures.Message._
import main.scala.structures.Poll
import structures.result._

class Worker(polls: PollContainer){
  def processQuery(user: String, query: Option[Query]): Option[Result] = query match {
    case Some(q) => q match {
      case x: CreatePollQuery => Some(createPoll(user, x))
      case x: ListQuery => Some(viewList(user, x))
      case x: DeletePollQuery => Some(deletePoll(user, x))
      case x: StartPollQuery => Some(startPoll(user, x))
      case x: EndPollQuery => Some(endPoll(user, x))
      case x: ViewResultQuery => Some(viewResult(user, x))
    }
    case None => None
  }

  def createPoll(user: String, q: CreatePollQuery): Result = {
    val id = polls.add(new Poll(user, q.name, q.isAnon.getOrElse(true),
      q.isVisible.getOrElse(false), q.startTime, q.stopTime))
    new PollCreated(id)
  }

  def viewList(user: String, q: ListQuery): Result = new ViewList(polls.toList)

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
      if (p.ended) AlreadyEnded
      else if (p.started) AlreadyStarted
      else if (p.start_time.isDefined) StartedByTimer
      else {
        polls.start(id)
        PollStarted
      }
    })

  def endPoll(user: String, q: EndPollQuery): Result =
    wrapPollFunc(user, q, id => {
      val p = polls.get(id).get
      if (p.ended) AlreadyEnded
      else if (!p.started) NotYetStarted
      else if (p.end_time.isDefined) EndedByTimer
      else {
        polls.end(id)
        PollEnded
      }
    })

  def viewResult(user: String, q: ViewResultQuery): Result = {
    polls.get(q.id) match {
      case None => NotFound
      case Some(p) =>
        if(!p.started) NotYetStarted
        else if(!p.ended && !p.isVisible) IsNotVisible
        else new ViewResult(p)
    }
  }
}

