package structures

object Message extends Enumeration {
  type Message = Value
  val NoRights, PollDeleted, PollStarted, PollEnded,
  AlreadyStarted, AlreadyEnded, NotYetEnded, NotYetStarted,
  StartedByTimer, EndedByTimer, NotFound, IsNotVisible, NotRecognized = Value
}
