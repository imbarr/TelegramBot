package structures

import java.util.{Calendar, Date}

case class Poll(user: String, name: String, isAnon: Boolean = true, isVisible: Boolean = false,
                start_time: Option[Date] = None, stop_time: Option[Date] = None,
                started: Boolean = false, stopped: Boolean = false) {

  def updated(): Poll = {
    val now = Calendar.getInstance().getTime
    Poll(user, name, isAnon, isVisible, start_time, stop_time,
      started || start_time.isDefined && start_time.get.before(now),
      stopped || stop_time.isDefined && stop_time.get.before(now))
  }

  def start(): Poll = Poll(user, name, isAnon, isVisible, start_time, stop_time, started=true)
  def end(): Poll = Poll(user, name, isAnon, isVisible, start_time, stop_time, stopped=true)
}

