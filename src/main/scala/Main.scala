import container.PollMemoryContainer
import info.mukel.telegrambot4s.models.User
import structures.query.Query

import scala.io.Source

object Main{
  def main(args: Array[String]): Unit = {
    val parser = new CommandParser()
    val polls = new PollMemoryContainer()
    val worker = new Worker(polls)
    val printer = new Printer()

    def inputToOutput(user: User, in: String): String ={
      val result = parser.parse(in).fold(
        x => x, (x: Query) => worker.processQuery(user, x))
      printer.get(result)
    }

    if(args.isDefinedAt(0) && (args(0) == "-t" || args(0) == "--test")) {
      val stream = getClass.getResourceAsStream("/input.txt")
      for (command <- Source.fromInputStream(stream).mkString.split(System.lineSeparator() * 2)) {
        print(inputToOutput(User(command.split(":")(0).toInt, true, "Smith"),
          command.split(":").drop(1).mkString(":")) + "\n\n")
      }
    }
    else{
      sys.env.get("IMBARR_POLL_BOT_TOKEN") match{
        case None => print("Fatal: failed to obtain token.")
        case Some(t) =>
          val bot = new PollingBot(t, parser.parsers.keys, inputToOutput)
          bot.run()
      }
    }
  }
}