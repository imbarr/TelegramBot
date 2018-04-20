package container

import structures.{Poll, Question}

trait PollContainer {
  def get(key: Int): Option[Poll]
  def add(value: Poll): Int
  def toList: List[(Int, Poll)]
  def delete(key: Int): Option[Poll]

  def start(key: Int): Unit
  def stop(key: Int): Unit
  def addQuestion(key: Int, question: Question): Unit
}


