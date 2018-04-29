package structures.question

import info.mukel.telegrambot4s.models.User

sealed trait Question{
  val isAnon: Boolean
  val question: String
  val voted: List[User]
}

case class ChoiceQuestion(question: String, options: List[String], isAnon: Boolean, voted: List[User] = Nil,
                          votes: List[(Option[User], Int)] = List()) extends Question{
  def answer(user: User, ans: Int): ChoiceQuestion =
    copy(voted = user +: voted, votes = (if(isAnon) None else Some(user), ans) +: votes)
}

case class MultipleQuestion(question: String, options: List[String], isAnon: Boolean, voted: List[User] = Nil,
                            votes: List[(Option[User], List[Int])] = List()) extends Question{
  def answer(user: User, ans: List[Int]): MultipleQuestion =
    copy(voted = user +: voted, votes = (if(isAnon) None else Some(user), ans) +: votes)
}

case class OpenQuestion(question: String, isAnon: Boolean, voted: List[User] = Nil,
                        votes: List[(Option[User], String)] = List()) extends Question{
  def answer(user: User, ans: String): OpenQuestion =
    copy(voted = user +: voted, votes = (if(isAnon) None else Some(user), ans) +: votes)
}