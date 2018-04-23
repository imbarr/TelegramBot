import org.scalatest._
import structures.query._
import structures.result.{MsgResult, ParseFailureResult}
import structures.Message.CommandNotFound

class CommandParserTests extends FlatSpec with Matchers {
  val p = new CommandParser()

  def isInstance[T](x: Any): Boolean = x match {
    case _: T => true
    case _ => false
  }

  def assertWrongFormat(s: String): Assertion = assert(isInstance[ParseFailureResult](p.parse(s).left))

  def assertWrongFormat(strings: Seq[String]): Unit = for(el <- strings) assertWrongFormat(el)

  def assertParsed(cmd: String, q: Query): Assertion = assert(p.parse(cmd).contains(q))
  def assertParsed(pairs: (String, Query)*): Unit =
    for(el <- pairs) assertParsed(el._1, el._2)

  "Parser" should "parse correct create_poll commands" in {
    val start = "11:40:59 02:01:30"
    val end = "23:40:59 07:01:29"
    assertParsed(
      "/create_poll (name)) abc(()))" ->
        CreatePollQuery("name) abc()"),
      "/create_poll (n) (yes)" ->
        CreatePollQuery("n", Some(true)),
      "/create_poll (n) (no) (afterstop)" ->
        CreatePollQuery("n", Some(false), Some(false)),
      "/create_poll (n) (no) (continuous) (" + start + ")" ->
        CreatePollQuery("n", Some(false), Some(true), Some(p.dateFormat.parse(start))),
      "/create_poll (n) (no) (continuous) (" + start + ") (" + end + ")" ->
        CreatePollQuery("n", Some(false), Some(true),
          Some(p.dateFormat.parse(start)), Some(p.dateFormat.parse(end))))
  }

  it should "not parse invalid create_poll commands" in assertWrongFormat(List(
    "(((name))", "(n) (some)", "(n) (continuous)",
    "(n) (no) (g)", "(n) (no) (afterstop) (time) (time)").map("/create_poll " + _))

  it should "parse list command" in isInstance[ListQuery](p.parse("/list"))

  it should "parse other correct poll commands" in assertParsed(
      "/delete_poll (99999)" -> DeletePollQuery(99999),
      "/start_poll (0)" -> StartPollQuery(0),
      "/stop_poll (10)" -> StopPollQuery(10),
      "/result (0)" -> ResultQuery(0))

  it should "not parse invalid poll commands" in {
    val cmds: List[String] = List("/delete_poll", "/start_poll", "/stop_poll", "/result")
    val args: List[String] = List("(-1)", "45", "(ab)")
    for{
      c <- cmds
      a <- args
    } assertWrongFormat(c + " " + a)
  }

  it should "not parse unrecognized commands" in
    assert(p.parse("/some_not_implemented_command").left.exists(_ == MsgResult(CommandNotFound)))

  it should "not parse not command strings" in assertWrongFormat("result")
}