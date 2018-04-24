package structures

import java.util.{Calendar, Date}

import structures.question.Question

case class Poll(user: String, name: String, isAnon: Boolean = true, isVisible: Boolean = false,
                start_time: Option[Date] = None, stop_time: Option[Date] = None,
                questions: List[Question] = Nil, manuallyStarted: Boolean = false,
                manuallyStopped: Boolean = false) {

  def started(now: Date): Boolean = manuallyStarted || (start_time.isDefined && start_time.get.before(now))

  def stopped(now: Date): Boolean = manuallyStopped || (stop_time.isDefined && stop_time.get.before(now))

  def start(): Poll = copy(manuallyStarted = true)

  def stop(): Poll = copy(manuallyStopped = true)

  def add(question: Question): Poll = copy(questions = questions :+ question)

  def delete(id: Int): Poll =
    copy(questions = questions.patch(id, Nil, 1))

  def set(id: Int, q: Question): Poll =
    copy(questions = questions.patch(id, Seq(q), 1))
}