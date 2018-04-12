package main.scala

import java.text.SimpleDateFormat
import java.util.Date

import main.scala.structures.query._

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

class Parser extends RegexParsers{
  def arg[T](p: Parser[T]): Parser[T] = "(" ~> p <~ ")"
  def optional[T](p: Parser[T]): Parser[Option[T]] = p ^^ (x => Some(x)) | """$""".r ^^ (_ => None)
  def optArg[T](p: Parser[T]): Parser[Option[T]] = optional(arg(p))

  def toDate(s: String): Option[Date] = Try(new SimpleDateFormat("HH:mm:ss yy:MM:dd").parse(s)).toOption

  val string: Parser[String] = """(\(\(|\)\)|[^()])*""".r ^^
    (s => s.replaceAll("""\(\(""", "(").replaceAll("""\)\)""", ")"))
  val int: Parser[Int] = "[0-9]+".r ^^ (p => p.toInt)
  val isAnon: Parser[Boolean] = ("yes" | "no") ^^ (_ == "yes")
  val isVisible: Parser[Boolean] = ("continuous" | "afterstop") ^^ (_ == "continuous")
  val date: Parser[Date] = "[^()]*".r ^? {case x if toDate(x).isDefined => toDate(x).get}

  val parsers: Parser[Query] =
    "/create_poll" ~ arg(string) ~ optArg(isAnon) ~ optArg(isVisible) ~ optArg(date) ~ optArg(date) ^^
      (x => new CreatePollQuery(x._1._1._1._1._2, x._1._1._1._2, x._1._1._2, x._1._2, x._2)) |
      "/list" ^^ (_ => new ListQuery) |
      "/delete_poll" ~ arg(int) ^^ (p => new DeletePollQuery(p._2)) |
      "/start_poll" ~ arg(int) ^^ (p => new StartPollQuery(p._2)) |
      "/stop_poll" ~ arg(int) ^^ (p => new EndPollQuery(p._2)) |
      "/result" ~ arg(int) ^^ (p => new ViewResultQuery(p._2))

  def getQuery(s: String): Option[Query] = parse(parsers, s) match{
    case Success(m, _) => Some(m)
    case Failure(_, _) => None
  }
}