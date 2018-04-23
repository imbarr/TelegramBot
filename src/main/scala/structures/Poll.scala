package structures

import java.util.{Calendar, Date}

import structures.question.Question

case class Poll(user: String, name: String, isAnon: Boolean = true, isVisible: Boolean = false,
                start_time: Option[Date] = None, stop_time: Option[Date] = None,
                questions: List[Question] = Nil, manuallyStarted: Boolean = false,
                manuallyStopped: Boolean = false) {

  def started(now: Date): Boolean = manuallyStarted || (start_time.isDefined && start_time.get.before(now))

  def stopped(now: Date): Boolean = manuallyStopped || (stop_time.isDefined && stop_time.get.before(now))

  def start(): Poll = copyWith(manuallyStarted = Some(true))

  def stop(): Poll = copyWith(manuallyStopped = Some(true))

  def add(question: Question): Poll = copyWith(questions = Some(questions :+ question))

  def delete(id: Int): Poll =
    copyWith(questions = Some(questions.patch(id, Nil, 1)))

  def set(id: Int, q: Question): Poll =
    copyWith(questions = Some(questions.patch(id, Seq(q), 1)))

  def copyWith(user: Option[String] = None,
               name: Option[String] = None,
               isAnon: Option[Boolean] = None,
               isVisible: Option[Boolean] = None,
               start_time: Option[Option[Date]] = None,
               stop_time: Option[Option[Date]] = None,
               questions: Option[List[Question]] = None,
               manuallyStarted: Option[Boolean] = None,
               manuallyStopped: Option[Boolean] = None): Poll =
    Poll(user.getOrElse(this.user),
      name.getOrElse(this.name),
      isAnon.getOrElse(this.isAnon),
      isVisible.getOrElse(this.isVisible),
      start_time.getOrElse(this.start_time),
      stop_time.getOrElse(this.stop_time),
      questions.getOrElse(this.questions),
      manuallyStarted.getOrElse(this.manuallyStarted),
      manuallyStopped.getOrElse(this.manuallyStopped))
}