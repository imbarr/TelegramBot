package structures

import java.util.{Calendar, Date}

case class Poll(user: String, name: String, isAnon: Boolean, isVisible: Boolean,
           start_time: Option[Date] = None, end_time: Option[Date] = None,
           started: Boolean = false, ended: Boolean = false) {

  def updated(): Poll = {
    val now = Calendar.getInstance().getTime
    Poll(user, name, isAnon, isVisible, start_time, end_time,
      start_time.isDefined && start_time.get.before(now),
      start_time.isDefined && start_time.get.before(now))
  }

  def start(): Poll = Poll(user, name, isAnon, isVisible, start_time, end_time, started=true)
  def end(): Poll = Poll(user, name, isAnon, isVisible, start_time, end_time, ended=true)
}

