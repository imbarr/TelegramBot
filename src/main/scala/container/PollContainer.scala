package container

import structures.{Poll, Question}

trait PollContainer {
  def get(key: Int): Option[Poll]
  def add(value: Poll): Int
  def set(key: Int, poll: Poll)
  def toList: List[(Int, Poll)]
  def delete(key: Int): Option[Poll]
}


