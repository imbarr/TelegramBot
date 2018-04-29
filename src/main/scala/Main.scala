import container.PollMemoryContainer
import structures.query.Query

import scala.io.Source

object Main{
  def main(args: Array[String]): Unit = {
    val parser = new CommandParser()
    val polls = new PollMemoryContainer()
    val worker = new Worker(polls)
    val printer = new Printer()

    def inputToOutput(user: String, in: String): String ={
      val result = parser.parse(in).fold(
        x => x, (x: Query) => worker.processQuery(user, x))
      printer.get(result)
    }

    if(args.isDefinedAt(0) && (args(0) == "-t" || args(0) == "--test")) {
      val stream = getClass.getResourceAsStream("/input.txt")
      for (command <- Source.fromInputStream(stream).mkString.split(System.lineSeparator() + System.lineSeparator())) {
        print(inputToOutput(command.split(":")(0), command.split(":").drop(1).mkString(":")) + "\n\n")
      }
    }
    else{
      val bot = new PollingBot(parser.parsers.keys, inputToOutput)
      bot.run()
    }
  }
}