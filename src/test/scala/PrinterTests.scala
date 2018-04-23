import org.scalatest.{FlatSpec, Matchers}
import structures.Message

class PrinterTests extends FlatSpec with Matchers{
  "Printer" should "support all messages" in {
    val p = new Printer()
    for (m <- Message.values)
      p.get(m)
  }
}
