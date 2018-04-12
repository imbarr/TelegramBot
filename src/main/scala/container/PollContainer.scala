package main.scala.container

import main.scala.structures.Poll

trait PollContainer {
  def get(key: Int): Option[Poll]
  def start(key: Int): Unit
  def end(key: Int): Unit
  def add(value: Poll): Int
  def toList: List[(Int, Poll)]
  def delete(key: Int): Option[Poll]
}


