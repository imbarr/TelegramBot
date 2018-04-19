package structures.query

import java.util.Date


sealed trait Query

case class CreatePollQuery(name: String, isAnon: Option[Boolean] = None, isVisible: Option[Boolean] = None,
                      startTime: Option[Date] = None, stopTime: Option[Date] = None) extends Query

class ViewListQuery extends Query

abstract sealed class QueryWithId(val id: Int) extends Query

case class DeletePollQuery(override val id: Int) extends QueryWithId(id)

case class StartPollQuery(override val id: Int) extends QueryWithId(id)

case class StopPollQuery(override val id: Int) extends QueryWithId(id)

case class ViewResultQuery(override val id: Int) extends QueryWithId(id)
