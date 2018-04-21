import java.text.SimpleDateFormat
import java.util.Date

import structures.QuestionType._
import structures.query._

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

class CommandParser extends RegexParsers{
  val dateFormat = new SimpleDateFormat("HH:mm:ss yy:MM:dd")

  def arg[T](p: Parser[T]): Parser[T] = "(" ~> p <~ ")"
  def optional[T](p: Parser[T]): Parser[Option[T]] = p ^^ (x => Some(x)) | """$""".r ^^ (_ => None)
  def fullyOptional[T](p: Parser[T]): Parser[Option[T]] = p ^^ (x => Some(x)) | "" ^^ (_ => None)
  def optArg[T](p: Parser[T]): Parser[Option[T]] = optional(arg(p))

  def toDate(s: String): Option[Date] = Try(dateFormat.parse(s)).toOption

  val string: Parser[String] = """(\(\(|\)\)|[^()])*""".r ^^
    (s => s.replaceAll("""\(\(""", "(").replaceAll("""\)\)""", ")"))
  val int: Parser[Int] = "[0-9]+".r ^^ (p => p.toInt)
  val isAnon: Parser[Boolean] = ("yes" | "no") ^^ (_ == "yes")
  val isVisible: Parser[Boolean] = ("continuous" | "afterstop") ^^ (_ == "continuous")
  val date: Parser[Date] = "[^()]*".r ^? {case x if toDate(x).isDefined => toDate(x).get}
  val questionType: Parser[QuestionType] =
    "open" ^^ (_ => Open) | "choice" ^^ (_ => Choice) | "multi" ^^ (_ => Multiple)

  val parsers: Parser[Query] =
    "/create_poll" ~ arg(string) ~ optArg(isAnon) ~ optArg(isVisible) ~ optArg(date) ~ optArg(date) ^^ {
      case _ ~ name ~ anon ~ visible ~ start ~ stop => CreatePollQuery(name, anon, visible, start, stop)} |
    "/list" ^^ (_ => new ListQuery) |
    "/delete_poll" ~ arg(int) ^^ (p => DeletePollQuery(p._2)) |
    "/start_poll" ~ arg(int) ^^ (p => StartPollQuery(p._2)) |
    "/stop_poll" ~ arg(int) ^^ (p => StopPollQuery(p._2)) |
    "/result" ~ arg(int) ^^ (p => ResultQuery(p._2)) |
    "/begin" ~ arg(int) ^^ (p => BeginQuery(p._2)) |
    "/end" ^^ (_ => new EndQuery()) |
    "/view" ^^ (_ => new ViewQuery()) |
    "/add_question" ~ arg(string) ~ fullyOptional(arg(questionType)) ~ optional("\n") ~
      (".+".r ^^ (o => o.split("\n").filter(_.isEmpty).toList)) ^^ {
        case _ ~ name ~ qtype ~ _ ~ list => AddQuestionQuery(name, qtype, list)} |
    "/delete_question" ~ arg(int) ^^ (p => DeleteQuestionQuery(p._2)) |
    "/answer" ~ arg(int) ~ arg(string) ^^ (p => AnswerQuestionQuery(p._1._2, p._2))



  def getQuery(s: String): Option[Query] = parse(parsers, s) match{
    case Success(m, _) => Some(m)
    case NoSuccess(_, _) => None
  }
}