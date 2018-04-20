package structures

import structures.QuestionType.QuestionType

case class Question(question: String, options: List[String], questionType: QuestionType)
