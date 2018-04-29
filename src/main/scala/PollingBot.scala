import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.api.declarative.Commands

class PollingBot(commands: Iterable[String], inputToOutput: (String, String) => String)
  extends TelegramBot with Polling with Commands{

  lazy val token = "Anime was a mistake"

  for(c <- commands)
    onCommand(c) {
      implicit msg => withArgs {
        args => inputToOutput("user", (c :+ args).mkString(" "))
      }
    }
}
