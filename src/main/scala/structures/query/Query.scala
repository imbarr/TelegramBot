package main.scala.structures.query

import java.util.Date

sealed trait Query

class CreatePollQuery(val name: String, val isAnon: Option[Boolean] = None, val isVisible: Option[Boolean] = None,
                      val startTime: Option[Date] = None, val stopTime: Option[Date] = None) extends Query {}

class ListQuery extends Query{}

abstract class QueryWithId(val id: Int) extends Query{}

class DeletePollQuery(override val id: Int) extends QueryWithId(id){}

class StartPollQuery(override val id: Int) extends QueryWithId(id){}

class EndPollQuery(override val id: Int) extends QueryWithId(id){}

class ViewResultQuery(override val id: Int) extends QueryWithId(id){}
