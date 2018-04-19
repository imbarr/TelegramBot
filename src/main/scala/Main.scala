import container.PollMemoryContainer

import scala.io.Source

object Main{
  def main(args: Array[String]): Unit = {
    val parser = new Parser()
    val polls = new PollMemoryContainer()
    val worker = new Worker(polls)
    val printer = new Printer()

    val stream = getClass.getResourceAsStream("/input.txt")
    for(l <- Source.fromInputStream(stream).getLines())
      print(printer.get(worker.processQuery("imbarr", parser.getQuery(l))).get + "\n")
  }
}

