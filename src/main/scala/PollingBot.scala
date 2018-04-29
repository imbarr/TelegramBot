import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.models.User

class PollingBot(telegramToken: String, commands: Iterable[String], inputToOutput: (User, String) => String)
  extends TelegramBot with Polling with Commands{

  lazy val token: String = telegramToken

  for(c <- commands)
    onCommand(c) { implicit msg => reply(inputToOutput(msg.from.get, msg.text.get)) }
}
