import org.scalatest._
import structures.query._
import structures.result.{MsgResult, ParseFailureResult}
import structures.Message.CommandNotFound
import structures.QuestionType._

class CommandParserTests extends FlatSpec with Matchers {
  val p = new CommandParser()

  def isInstance[T](x: Any): Boolean = x match {
    case _: T => true
    case _ => false
  }

  def assertWrongFormat(s: String): Assertion = assert(isInstance[ParseFailureResult](p.parse(s).left))

  def assertWrongFormat(strings: Seq[String]): Unit = for(el <- strings) assertWrongFormat(el)

  def assertParsed(cmd: String, q: Query): Assertion = assert(p.parse(cmd).contains(q))
  def assertParsed(pairs: List[(String, Query)]): Unit =
    for(el <- pairs) assertParsed(el._1, el._2)

  "Parser" should "parse correct create_poll commands" in {
    val start = "11:40:59 02:01:30"
    val end = "23:40:59 07:01:29"
    assertParsed(List(
      "(name)) abc(()))" -> CreatePollQuery("name) abc()"),
      "(n) (yes)" -> CreatePollQuery("n", Some(true)),
      "(n) (no) (afterstop)" -> CreatePollQuery("n", Some(false), Some(false)),
      "(n) (no) (continuous) (" + start + ")" ->
        CreatePollQuery("n", Some(false), Some(true), Some(p.dateFormat.parse(start))),
      "(n) (no) (continuous) (" + start + ") (" + end + ")" ->
        CreatePollQuery("n", Some(false), Some(true),
          Some(p.dateFormat.parse(start)), Some(p.dateFormat.parse(end)))
    ).map(p => ("/create_poll " + p._1, p._2)))
  }

  it should "not parse invalid create_poll commands" in assertWrongFormat(List(
    "(((name))", "(n) (some)", "(n) (continuous)",
    "(n) (no) (g)", "(n) (no) (afterstop) (time) (time)").map("/create_poll " + _))

  it should "parse correct add_question commands" in assertParsed(List(
    "(name)\n1\n2\n3" -> AddQuestionQuery("name", options=List("1", "2", "3")),
    "(n) (open)" -> AddQuestionQuery("n", Some(Open)),
    "(n) (choice)\n1" -> AddQuestionQuery("n", Some(Choice), List("1")),
    "(n) (multi)\n1\nabc" -> AddQuestionQuery("n", Some(Multiple), List("1", "abc")),
  ).map(p => ("/add_question " + p._1, p._2)))

  it should "not parse invalid add_question commands" in{
    assertWrongFormat(List("/add_question (n) (asas)", "/add_question (n) (open) (asas)"))
  }

  it should "parse commands with no arguments" in {
    isInstance[ListQuery](p.parse("/list"))
    isInstance[EndQuery](p.parse("/end"))
    isInstance[ViewQuery](p.parse("/view"))
  }

  it should "parse other correct poll commands" in assertParsed(List(
      "/delete_poll (99999)" -> DeletePollQuery(99999),
      "/start_poll (0)" -> StartPollQuery(0),
      "/stop_poll (10)" -> StopPollQuery(10),
      "/result (0)" -> ResultQuery(0),
      "/begin (76)" -> BeginQuery(76),
      "/delete_question (3)" -> DeleteQuestionQuery(3),
      "/answer (9) (abcd)" -> AnswerQuestionQuery(9, "abcd")))

  it should "not parse invalid poll commands" in {
    val cmds: List[String] = List("/delete_poll", "/start_poll",
      "/stop_poll", "/result", "/begin", "/delete_question")
    val args: List[String] = List("(-1)", "45", "(ab)")
    for{
      c <- cmds
      a <- args
    } assertWrongFormat(c + " " + a)
  }

  it should "not parse unrecognized commands" in
    assert(p.parse("/some_not_implemented_command").left.exists(_ == MsgResult(CommandNotFound)))
}