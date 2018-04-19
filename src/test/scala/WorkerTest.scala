import container.PollMemoryContainer
import org.scalatest._
import structures.Poll
import structures.query._
import structures.query.ListQuery
import structures.result.{MsgResult, PollCreated, ViewList}
import structures.Message._

class WorkerTest extends FlatSpec with Matchers with BeforeAndAfter{
  val customPoll = Poll("user", "name", isAnon = true, isVisible = false)

  def msgResultTest(user: String, poll: Poll, query: Query, msg: Message, pred: Option[Poll] => Boolean): Unit =
    msgResultTest(user, poll, _ => query, msg, pred)

  def msgResultTest(user: String, poll: Poll, query: Int => Query,
                    msg: Message, pred: Option[Poll] => Boolean): Unit =
    resultTest[MsgResult](user, poll, query, (res: MsgResult, p: Option[Poll]) => res.msg == msg && pred(p))

  def resultTest[T](user: String, poll: Poll, query: Query, pred: (T, Option[Poll]) => Boolean): Unit =
    resultTest[T](user, poll, _: Int => Query, pred)

  def resultTest[T](user: String, poll: Poll, query: Int => Query, pred: (T, Option[Poll]) => Boolean): Unit = {
    val polls = new PollMemoryContainer()
    val id = polls.add(poll)
    val w = new Worker(polls)

    val result = w.processQuery(user, Some(query(id)))
    assert(result match {
      case res: T => pred(res, polls.get(id))
      case _ => false
    })
  }

  "Worker" should "create poll correctly" in resultTest[PollCreated]("user", customPoll,
    CreatePollQuery("name"), (res: PollCreated, p: Option[Poll]) => p.contains(customPoll))

  it should "delete poll correctly" in
    msgResultTest("user", customPoll, id => DeletePollQuery(id), PollDeleted, x => x.isEmpty)

  it should "start poll correctly" in msgResultTest("user", customPoll, id => StartPollQuery(id),
    PollStarted, x => x.get.started)

  it should "stop poll correctly" in msgResultTest("user", customPoll, id => StopPollQuery(id),
    PollEnded, x => x.get.ended)

  it should "view list of polls" in resultTest[ViewList]("user", customPoll,
    _ => new ListQuery(), (res: ViewList, p: Option[Poll]) => res.polls.length == 1)

  it should "return poll result" in
}
