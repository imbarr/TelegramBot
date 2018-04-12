package main.scala.structures

import java.util.{Calendar, Date}

class Poll(val user: String, val name: String, val isAnon: Boolean, val isVisible: Boolean,
           val start_time: Option[Date], val end_time: Option[Date],
           val started: Boolean = false, val ended: Boolean = false) {

  def updated(): Poll = {
    val now = Calendar.getInstance().getTime
    new Poll(user, name, isAnon, isVisible, start_time, end_time,
      start_time.isDefined && start_time.get.before(now),
      start_time.isDefined && start_time.get.before(now))
  }

  def start(): Poll = new Poll(user, name, isAnon, isVisible, start_time, end_time, true, false)
  def end(): Poll = new Poll(user, name, isAnon, isVisible, start_time, end_time, true, true)
}

