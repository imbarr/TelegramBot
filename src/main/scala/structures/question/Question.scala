package structures.question

import scala.util.parsing.combinator.RegexParsers

sealed trait Question{
  val question: String
  val voted: List[String]
  def addAnswer(user: String, answer: String, anonymous: Boolean): Option[Question]
}

abstract class AbstractQuestion[T] extends Question with RegexParsers {
  val votes: Map[Option[String], T]
  val toAnswer: Parser[T]
  def fromAnswer(user: String, answer: T, anonymous: Boolean): Question

  override def addAnswer(user: String, answer: String, anonymous: Boolean): Option[Question] =
    parse(toAnswer, answer) match {
      case Success(x, _) =>
        if(voted.contains(user)) None
        else Some(fromAnswer(user, x, anonymous))
      case NoSuccess(_, _) => None
    }
}

case class ChoiceQuestion(question: String, options: List[String], override val voted: List[String] = Nil,
                          votes: Map[Option[String], Int] = Map()) extends AbstractQuestion[Int] {

  val toAnswer: Parser[Int] = "[0-9]+".r ^^ (p => p.toInt - 1) ^?
    {case x if x < options.length && x > 0 => x}

  override def fromAnswer(user: String, answer: Int, anonymous: Boolean): ChoiceQuestion =
    ChoiceQuestion(question, options, voted :+ user, votes + ((if (anonymous) None else Some(user)) -> answer))
}

case class MultipleQuestion(question: String, options: List[String], voted: List[String] = Nil,
                            votes: Map[Option[String], List[Int]] = Map()) extends AbstractQuestion[List[Int]] {

  val toAnswer: Parser[List[Int]] = "([0-9] )*[0-9]".r ^^
    (s => s.split(" ").map(_.toInt - 1).toList) ^?
    {case x if x.distinct.lengthCompare(x.length) == 0 && x.forall(i => i < options.length && i > 0) => x}

  override def fromAnswer(user: String, answer: List[Int], anonymous: Boolean): MultipleQuestion =
    MultipleQuestion(question, options, voted :+ user, votes + ((if (anonymous) None else Some(user)) -> answer))
}

case class OpenQuestion(question: String, voted: List[String] = Nil,
                        votes: Map[Option[String], String] = Map()) extends AbstractQuestion[String] {

  val toAnswer: Parser[String] = ".*".r

  override def fromAnswer(user: String, answer: String, anonymous: Boolean): OpenQuestion =
    OpenQuestion(question, voted :+ user, votes + ((if (anonymous) None else Some(user)) -> answer))
}