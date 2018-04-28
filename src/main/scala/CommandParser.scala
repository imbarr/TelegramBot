import java.text.SimpleDateFormat
import java.util.Date

import structures.QuestionType._
import structures.query._
import structures.result.{ParseFailureResult, Result}
import structures.Message.CommandNotFound

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class CommandParser extends RegexParsers {
  val dateFormat = new SimpleDateFormat("HH:mm:ss yy:MM:dd")

  override val whiteSpace: Regex = """[ \r]+""".r

  def arg[T](p: Parser[T]): Parser[T] = "(" ~> p <~ ")"

  def optional[T](p: Parser[T]): Parser[Option[T]] = p ^^ (x => Some(x)) | """$""".r ^^ (_ => None)

  def fullyOptional[T](p: Parser[T]): Parser[Option[T]] = p ^^ (x => Some(x)) | "" ^^ (_ => None)

  def optArg[T](p: Parser[T]): Parser[Option[T]] = optional(arg(p))

  def toDate(s: String): Option[Date] = Try(dateFormat.parse(s)).toOption

  def id(f: Int => Query): Parser[Query] = arg(int) ^^ f

  val string: Parser[String] =
    """(\(\(|\)\)|[^()])*""".r ^^
      (s => s.replaceAll("""\(\(""", "(").replaceAll("""\)\)""", ")"))
  val int: Parser[Int] = "[0-9]+".r ^^ (p => p.toInt)
  val isAnon: Parser[Boolean] = ("yes" | "no") ^^ (_ == "yes")
  val isVisible: Parser[Boolean] = ("continuous" | "afterstop") ^^ (_ == "continuous")
  val date: Parser[Date] = "[^()]*".r ^? { case x if toDate(x).isDefined => toDate(x).get }
  val questionType: Parser[QuestionType] =
    "open" ^^ (_ => Open) | "choice" ^^ (_ => Choice) | "multi" ^^ (_ => Multiple)
  val lines: Parser[List[String]] = "(?s).*".r ^^ (o => o.split("\n|\r").filter(_.nonEmpty).toList)

  val parsers: Map[String, Parser[Query]] = Map(
    "/create_poll" ->
      (arg(string) ~ optArg(isAnon) ~ optArg(isVisible) ~ optArg(date) ~ optArg(date) ^^ {
        case name ~ anon ~ visible ~ start ~ stop => CreatePollQuery(name, anon, visible, start, stop)
      }),
    "/list" -> ("""$""".r ^^ (_ => new ListQuery())),
    "/delete_poll" -> id(DeletePollQuery),
    "/start_poll" -> id(StartPollQuery),
    "/stop_poll" -> id(StopPollQuery),
    "/result" -> id(ViewResultQuery),
    "/begin" -> id(BeginQuery),
    "/end" -> ("""$""".r ^^ (_ => new EndQuery())),
    "/view" -> ("""$""".r ^^ (_ => new ViewQuery())),
    "/add_question" ->
      (arg(string) ~ fullyOptional(arg(questionType)) ~ optional("\n") ~ lines ^^ {
        case name ~ qtype ~ _ ~ list => AddQuestionQuery(name, qtype, list)
      }),
    "/delete_question" -> id(DeleteQuestionQuery),
    "/answer" ->
      (arg(int) ~ arg(string) ^^ (p => AnswerQuestionQuery(p._1, p._2))))

  def parse(s: String): Either[Result, Query] = parsers.get(s.split(" ")(0)) match {
    case Some(p) => parse(p ~ """$""".r, s.split(" ").drop(1).mkString(" ")) match {
      case Success(q, _) => Right(q._1)
      case NoSuccess(msg, x) => Left(ParseFailureResult(msg, x.pos.column))
    }
    case None => Left(CommandNotFound)
  }
}