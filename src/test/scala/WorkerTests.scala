import java.util.{Calendar, Date}

import container.PollMemoryContainer
import org.scalatest._
import structures.Poll
import structures.QuestionType._
import structures.query._
import structures.query.ListQuery
import structures.result._
import structures.Message._
import structures.QuestionType.QuestionType
import structures.question.{ChoiceQuestion, MultipleQuestion, OpenQuestion, Question}

import scala.reflect.ClassTag

class WorkerTests extends FlatSpec with Matchers{
  val somePoll = Poll("user", "name")
  val now: Date = Calendar.getInstance().getTime
  val past: Date = Calendar.getInstance().getTime
  past.setYear(past.getYear - 1)
  val future: Date = Calendar.getInstance().getTime
  future.setYear(future.getYear + 1)

  def isCorrectTypeAnd[T <: Result: ClassTag](instance: Result, predicate: T => Boolean): Boolean =
    instance match {
      case x: T => predicate(x)
      case _ => false
    }

  def isCorrectMsg(instance: Result, msg: Message): Boolean =
    isCorrectTypeAnd[MsgResult](instance, x => x.msg == msg)

  def processAndAssertMsg(w: Worker, q: Query, msg: Message): Assertion = {
    val res = w.processQuery("user", q)
    assert(isCorrectMsg(res, msg))
  }

  case class Data(polls: PollMemoryContainer, id: Int, w: Worker)

  def init(poll: Poll): Data = {
    val polls = new PollMemoryContainer()
    Data(polls, polls.add(poll), new Worker(polls))
  }

  def initContext(poll: Poll): Data = {
    val d = init(poll)
    d.w.currentPoll = Some(d.id)
    d
  }

  "create_poll" should "work correctly" in {
    val polls = new PollMemoryContainer()
    val w = new Worker(polls)
    val res = w.processQuery("u", CreatePollQuery("n"))
    assert(isCorrectTypeAnd[PollCreated](res, x => polls.get(x.pollId).contains(Poll("u", "n"))))
  }

  def customPollTest(f: Int => QueryWithId, poll: Poll, msg: Message,
                     predicate: Option[Poll] => Boolean): Assertion = {
    val d = init(poll)
    val res = d.w.processQuery(poll.user, f(d.id))
    assert(isCorrectMsg(res, msg) && predicate(d.polls.get(d.id)))
  }

  def customQuestionTest(f: Int => QueryWithQuestionId, question: Question, msg: Message,
                         predicate: Option[Question] => Boolean): Assertion ={
    val p = Poll("u", "n")
    val d = initContext(p)
    d.polls.set(d.id, p.add(question))
    val res = d.w.processQuery("u", f(0))
    assert(isCorrectMsg(res, msg) && predicate(d.polls.get(d.id).get.get(0)))
  }

  "delete_poll" should "work correctly" in
    customPollTest(DeletePollQuery, somePoll, PollDeleted, _.isEmpty)

  "start_poll" should "work correctly" in
    customPollTest(StartPollQuery, Poll("u", "n"), PollStarted, _.get.started(now))

  "stop_poll" should "work correctly" in
    customPollTest(StopPollQuery, Poll("u", "n", manuallyStarted = true), PollStopped, _.get.stopped(now))

  "list" should "view list of polls" in {
    val polls = new PollMemoryContainer()
    polls.add(somePoll)
    polls.add(somePoll)
    val w = new Worker(polls)

    val res = w.processQuery("u", new ListQuery())
    assert(isCorrectTypeAnd[ViewList](res, x => x.polls.lengthCompare(2) == 0))
  }

  "view_result" should "view poll result" in {
    val d = init(Poll("u", "n", manuallyStarted = true, isVisible = true))
    val res = d.w.processQuery("u", ResultQuery(d.id))
    assert(isCorrectTypeAnd[ViewResult](res, _ => true))
  }

  "NotFound message" should "be returned" in {
    val queries = List[Query](DeletePollQuery(0), StartPollQuery(0), StopPollQuery(0),
      ResultQuery(0), BeginQuery(0))
    val polls = new PollMemoryContainer()
    val w = new Worker(polls)
    for(q <- queries)
      processAndAssertMsg(w, q, NotFound)
  }

  it should "be returned while in context" in{
    val queries = List[Query](new ViewQuery(), AddQuestionQuery("a"),
      DeleteQuestionQuery(0), AnswerQuestionQuery(0, "a"))
    val polls = new PollMemoryContainer()
    val w = new Worker(polls, Some(0))
    for(q <- queries)
      processAndAssertMsg(w, q, NotFound)
  }

  "NoRights message" should "be returned" in {
    val d = initContext(Poll("admin", "n"))
    val queries = List[Query](DeletePollQuery(d.id), StartPollQuery(d.id), StopPollQuery(d.id),
      AddQuestionQuery("a"))
    for(q <- queries)
      processAndAssertMsg(d.w, q, NoRights)
  }

  "start_poll"  should "return AlreadyEnded" in
    customPollTest(StartPollQuery, Poll("u", "n", manuallyStopped =true), AlreadyStopped, _ => true)

  it should "return AlreadyStarted" in
    customPollTest(StartPollQuery, Poll("u", "n", manuallyStarted = true), AlreadyStarted, _ => true)

  it should "return StartedByTimer" in
    customPollTest(StartPollQuery, Poll("u", "n", start_time = Some(future)), StartedByTimer, _ => true)

  "stop_poll" should "return AlreadyEnded" in
    customPollTest(StopPollQuery, Poll("u", "n", manuallyStopped = true), AlreadyStopped, _ => true)

  it should "return NotYetStarted" in
    customPollTest(StopPollQuery, Poll("u", "n"), NotYetStarted, _ => true)

  it should "return StoppedByTimer" in
    customPollTest(StopPollQuery,
      Poll("u", "n", manuallyStarted = true, stop_time = Some(future)), StoppedByTimer, _ => true)

  "view_result" should "return NotYetStarted" in
    customPollTest(ResultQuery, Poll("u", "n", isVisible = true), NotYetStarted, _ => true)

  it should "return IsNotVisible" in
    customPollTest(ResultQuery, Poll("u", "n", manuallyStarted = true), IsNotVisible, _ => true)

  "begin" should "work correctly" in{
    val d = init(Poll("u", "n"))
    processAndAssertMsg(d.w, BeginQuery(d.id), Begin)
    assert(d.w.currentPoll.contains(d.id))
  }

  it should "return AlreadyInContext" in{
    val d = initContext(Poll("u", "n"))
    processAndAssertMsg(d.w, BeginQuery(d.id), AlreadyInContext)
  }

  "NotInContext message" should "be returned" in{
    val d = init(Poll("user", "n"))
    val queries = List[Query](new EndQuery(), new ViewQuery(),
      AddQuestionQuery("a"), DeleteQuestionQuery(0), AnswerQuestionQuery(0, "a"))
    for(q <- queries)
      processAndAssertMsg(d.w, q, NotInContext)
  }

  "end" should "work correctly" in{
    val d = initContext(Poll("u", "n"))
    processAndAssertMsg(d.w, new EndQuery(), End)
    assert(d.w.currentPoll.isEmpty)
  }

  "view" should "work correctly" in{
    val d = initContext(Poll("u", "n"))
    val res = d.w.processQuery("user", new ViewQuery())
    assert(isCorrectTypeAnd[ViewResult](res, _.poll == Poll("u", "n")))
  }

  def customAddQuestionTest(name: String, qtype: Option[QuestionType], options: List[String],
                            f: (String, List[String]) => Question): Unit ={
    val d = initContext(Poll("u", "n"))
    val res = d.w.processQuery("u", AddQuestionQuery(name, qtype, options))
    assert(isCorrectTypeAnd[QuestionAdded](res, r =>
      d.polls.get(d.id).get.questions(r.questionId) == f(name, options)))
  }

  "add_question" should "work correctly" in{
    customAddQuestionTest("a", None, List("1"), (n, l) => ChoiceQuestion(n, l))
    customAddQuestionTest("a", Some(Choice), List("1"), (n, l) => ChoiceQuestion(n, l))
    customAddQuestionTest("a", Some(Multiple), List("1"), (n, l) => MultipleQuestion(n, l))
    customAddQuestionTest("a", Some(Open), Nil, (n, _) => OpenQuestion(n))
  }

  def customAddQuestionMsgTest(name: String, qtype: Option[QuestionType],
                               options: List[String], msg: Message): Unit = {
    val d = initContext(Poll("u", "n"))
    val res = d.w.processQuery("u", AddQuestionQuery(name, qtype, options))
    assert(isCorrectMsg(res, msg))
  }

  it should "return NoOptions" in{
    customAddQuestionMsgTest("a", Some(Choice), Nil, NoOptions)
    customAddQuestionMsgTest("a", Some(Multiple), Nil, NoOptions)
  }

  it should "return NoOptionsExpected" in
    customAddQuestionMsgTest("a", Some(Open), List("1"), NoOptionsExpected)

  "delete_question" should "work correctly" in
    customQuestionTest(DeleteQuestionQuery, OpenQuestion("a"), QuestionDeleted, _.isEmpty)

  "QuestionNotFound" should "be returned" in{
    val d = initContext(Poll("user", "n"))
    val queries = List[Query](DeleteQuestionQuery(0)/*, AnswerQuestionQuery(0, "a")*/)
    for(q <- queries)
      processAndAssertMsg(d.w, q, QuestionNotFound)
  }

  "answer_question" should "work correctly" in{
    customQuestionTest(id => AnswerQuestionQuery(id, "answer"),
      OpenQuestion("a"), Answered, _ => true)
    //customQuestionTest(id => AnswerQuestionQuery(id, "1"),
      //MultipleQuestion("a", List("a", "b", "c")), Answered, _ => true)
    customQuestionTest(id => AnswerQuestionQuery(id, "1"),
      ChoiceQuestion("a", List("a", "b")), Answered, _ => true)
  }

  it should "return AlreadyAnswered" in
    customQuestionTest(id => AnswerQuestionQuery(id, "a"),
      OpenQuestion("a", voted = List("u")), AlreadyAnswered, _ => true)

  it should "return WrongAnswerFormat" in{
    def choiceFunc(answer: String) =
      customQuestionTest(id => AnswerQuestionQuery(id, answer),
        ChoiceQuestion("a", List("1")), WrongAnswerFormat, _ => true)

    def multipleFunc(answer: String) =
      customQuestionTest(id => AnswerQuestionQuery(id, answer),
        MultipleQuestion("a", List("1")), WrongAnswerFormat, _ => true)

    choiceFunc("-1")
    choiceFunc("2")
    choiceFunc("1 2")
    multipleFunc("-1")
    multipleFunc("1 2")
    multipleFunc("1 1")
    multipleFunc("abc")
  }
}
