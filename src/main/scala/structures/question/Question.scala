package structures.question

sealed trait Question{
  val isAnon: Boolean
  val question: String
  val voted: List[String]
}

case class ChoiceQuestion(question: String, options: List[String], isAnon: Boolean, voted: List[String] = Nil,
                          votes: List[(Option[String], Int)] = List()) extends Question{
  def answer(user: String, ans: Int): ChoiceQuestion =
    copy(voted = user +: voted, votes = (if(isAnon) None else Some(user), ans) +: votes)
}

case class MultipleQuestion(question: String, options: List[String], isAnon: Boolean, voted: List[String] = Nil,
                            votes: List[(Option[String], List[Int])] = List()) extends Question{
  def answer(user: String, ans: List[Int]): MultipleQuestion =
    copy(voted = user +: voted, votes = (if(isAnon) None else Some(user), ans) +: votes)
}

case class OpenQuestion(question: String, isAnon: Boolean, voted: List[String] = Nil,
                        votes: List[(Option[String], String)] = List()) extends Question{
  def answer(user: String, ans: String): OpenQuestion =
    copy(voted = user +: voted, votes = (if(isAnon) None else Some(user), ans) +: votes)
}