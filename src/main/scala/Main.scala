import container.PollMemoryContainer
import structures.query.Query
import structures.result.Result

import scala.io.Source

object Main{

  def main(args: Array[String]): Unit = {
    val parser = new CommandParser()
    val polls = new PollMemoryContainer()
    val worker = new Worker(polls)
    val printer = new Printer()

    def inputToOutput(in: String): String ={
      val result = parser.parse(in).fold(x => x, (x: Query) => worker.processQuery("user", x))
      printer.get(result)
    }

    val stream = getClass.getResourceAsStream("/input.txt")
    for(command <- Source.fromInputStream(stream).mkString.split("\n\n")){
        print(inputToOutput(command)+ "\n\n")
    }
  }
}

