package structures.query

import java.util.Date

import structures.QuestionType.QuestionType


sealed trait Query

case class CreatePollQuery(name: String, isAnon: Option[Boolean] = None, isVisible: Option[Boolean] = None,
                      startTime: Option[Date] = None, stopTime: Option[Date] = None) extends Query

class ViewListQuery extends Query

class EndQuery extends Query

class ViewPollQuery extends Query

abstract sealed class QueryWithId(val id: Int) extends Query

case class DeletePollQuery(override val id: Int) extends QueryWithId(id)

case class StartPollQuery(override val id: Int) extends QueryWithId(id)

case class StopPollQuery(override val id: Int) extends QueryWithId(id)

case class ViewResultQuery(override val id: Int) extends QueryWithId(id)

case class BeginQuery(override val id: Int) extends QueryWithId(id)

abstract sealed class QueryWithQuestionId(val id: Int) extends Query

case class AddQuestionQuery(override val id: Int, question: String, questionType: Option[QuestionType] = None,
                            options: List[String] = List()) extends QueryWithQuestionId(id)

case class DeleteQuestionQuery(override val id: Int) extends QueryWithQuestionId(id)

case class AnswerQuestionQuery(override val id: Int, answer: String) extends QueryWithQuestionId(id)