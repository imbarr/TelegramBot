import java.util.{Calendar, Date}

import container.PollMemoryContainer
import org.scalatest._
import structures.Poll
import structures.query._
import structures.query.ViewListQuery
import structures.result._
import structures.Message._

class WorkerTests extends FlatSpec with Matchers with BeforeAndAfter{
  val somePoll = Poll("user", "name")
  val past: Date = Calendar.getInstance().getTime
  past.setYear(past.getYear - 1)
  val future: Date = Calendar.getInstance().getTime
  future.setYear(future.getYear + 1)

  def isCorrectTypeAnd[T <: Result](instance: Result, predicate: T => Boolean): Boolean = instance match {
    case x: T => predicate(x)
    case _ => false
  }

  def isCorrectMsg(instance: Result, msg: Message): Boolean =
    isCorrectTypeAnd[MsgResult](instance, x => x.msg == msg)

  def processAndAssertMsg(w: Worker, q: Query, msg: Message): Assertion = {
    val res = w.processQuery("user", Some(q))
    assert(isCorrectMsg(res, msg))
  }

  case class Data(polls: PollMemoryContainer, id: Int, w: Worker)

  def init(poll: Poll): Data = {
    val polls = new PollMemoryContainer()
    Data(polls, polls.add(poll), new Worker(polls))
  }

  "Worker" should "create poll correctly" in {
    val polls = new PollMemoryContainer()
    val w = new Worker(polls)
    val res = w.processQuery("u", Some(CreatePollQuery("n")))
    assert(isCorrectTypeAnd[PollCreated](res, x => polls.get(x.pollId).contains(Poll("u", "n"))))
  }

  def customPollTest(f: Int => QueryWithId, poll: Poll, msg: Message, predicate: Option[Poll] => Boolean): Assertion = {
    val d = init(poll)
    val res = d.w.processQuery(poll.user, Some(f(d.id)))
    assert(isCorrectMsg(res, msg) && predicate(d.polls.get(d.id)))
  }

  "delete_poll" should "work correctly" in
    customPollTest(DeletePollQuery, somePoll, PollDeleted, _.isEmpty)

  "start_poll" should "work correctly" in
    customPollTest(StartPollQuery, Poll("u", "n"), PollStarted, _.get.started)

  "stop_poll" should "work correctly" in
    customPollTest(StopPollQuery, Poll("u", "n", started = true), PollStopped, _.get.stopped)

  "list" should "view list of polls" in {
    val polls = new PollMemoryContainer()
    polls.add(somePoll)
    polls.add(somePoll)
    val w = new Worker(polls)

    val res = w.processQuery("u", Some(new ViewListQuery()))
    assert(isCorrectTypeAnd[ViewList](res, x => x.polls.lengthCompare(2) == 0))
  }

  "view_result" should "view poll result" in {
    val d = init(Poll("u", "n", started=true, isVisible = true))
    val res = d.w.processQuery("u", Some(ViewResultQuery(d.id)))
    assert(isCorrectTypeAnd[ViewResult](res, x => true))
  }

  "NotFound message" should "be returned" in {
    val queries = List[Query](DeletePollQuery(0), StartPollQuery(0), StopPollQuery(0), ViewResultQuery(0))
    val polls = new PollMemoryContainer()
    val w = new Worker(polls)
    for(q <- queries)
      processAndAssertMsg(w, q, NotFound)
  }

  "NoRights message" should "be returned" in {
    val d = init(Poll("admin", "n"))
    val queries = List[Query](DeletePollQuery(d.id), StartPollQuery(d.id), StopPollQuery(d.id))
    for(q <- queries)
      processAndAssertMsg(d.w, q, NoRights)
  }

  "start_poll"  should "return AlreadyEnded" in
    customPollTest(StartPollQuery, Poll("u", "n", stopped =true), AlreadyStopped, _ => true)

  it should "return AlreadyStarted" in
    customPollTest(StartPollQuery, Poll("u", "n", started = true), AlreadyStarted, _ => true)

  it should "return StartedByTimer" in
    customPollTest(StartPollQuery, Poll("u", "n", start_time = Some(future)), StartedByTimer, _ => true)

  "stop_poll" should "return AlreadyEnded" in
    customPollTest(StopPollQuery, Poll("u", "n", stopped = true), AlreadyStopped, _ => true)

  it should "return NotYetStarted" in
    customPollTest(StopPollQuery, Poll("u", "n"), NotYetStarted, _ => true)

  it should "return StoppedByTimer" in
    customPollTest(StopPollQuery, Poll("u", "n", started = true, stop_time = Some(future)), StoppedByTimer, _ => true)

  "view_result" should "return NotYetStarted" in
    customPollTest(ViewResultQuery, Poll("u", "n", isVisible = true), NotYetStarted, _ => true)

  it should "return IsNotVisible" in
    customPollTest(ViewResultQuery, Poll("u", "n", started = true), IsNotVisible, _ => true)

  "None" should "return NotRecognized" in {
    val w = new Worker(new PollMemoryContainer())
    assert(isCorrectMsg(w.processQuery("u", None), NotRecognized))
  }
}
