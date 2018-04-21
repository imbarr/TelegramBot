package structures

object Message extends Enumeration {
  type Message = Value
  val NoRights, PollDeleted, PollStarted, PollStopped,
  AlreadyStarted, AlreadyStopped, NotYetEnded, NotYetStarted,
  StartedByTimer, StoppedByTimer, NotFound, IsNotVisible, NotRecognized,
  Begin, End, AlreadyInContext, NotInContext, NoOptions, NoOptionsExpected,
  QuestionNotFound, QuestionDeleted, Answered, WrongAnswerFormat,
  AlreadyAnswered = Value
}
