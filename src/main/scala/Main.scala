import container.PollMemoryContainer
import structures.query.Query
import scala.io.Source

object Main{

  def main(args: Array[String]): Unit = {
    val parser = new CommandParser()
    val polls = new PollMemoryContainer()
    val worker = new Worker(polls)
    val printer = new Printer()

    def inputToOutput(in: String): String ={
      val result = parser.parse(in.split(":").drop(1).mkString("")).fold(
        x => x, (x: Query) => worker.processQuery(in.split(":")(0), x))
      printer.get(result)
    }

    val stream = getClass.getResourceAsStream("/input.txt")
    for(command <- Source.fromInputStream(stream).mkString.split(System.lineSeparator() + System.lineSeparator())){
        print(inputToOutput(command)+ "\n\n")
    }
  }
}

