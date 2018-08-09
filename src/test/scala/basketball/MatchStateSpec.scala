package basketball

import basketball.sanitization.EventSet
import org.scalatest.{FlatSpecLike, Matchers}

import scala.io.Source


class MatchStateSpec extends FlatSpecLike with Matchers with Parser {

  private def getNewMatchState() = {
    new MatchState {
      override def sanitize(pendingEvents: List[MatchEvent], lastEvent: Option[MatchEvent], strictSanitization: Boolean): EventSet = {
        EventSet(lastEvent.toList ++ pendingEvents, Nil)
      }
    }
  }

  it should "Get last event correctly" in {
    val eventsRaw = Source.fromResource("sample1.txt").getLines.toList
    val matchState = getNewMatchState
    eventsRaw.foreach { encodedEvent =>
      parse(encodedEvent).map(matchState.addEvent)
    }

    matchState.getLastEvent() shouldBe Some(MatchEvent(2,0,27,29,598))
  }

  it should "Get last 3 events correctly" in {
    val eventsRaw = Source.fromResource("sample2.txt").getLines.toList
    val matchState = getNewMatchState
    eventsRaw.foreach { encodedEvent =>
      parse(encodedEvent).map(matchState.addEvent)
    }

    matchState.getLastEvents(3) shouldBe List(
      MatchEvent(2,0,23,29,560), MatchEvent(2,0,25,29,579), MatchEvent(3,0,232,234,1500)
    )
  }

  it should "Get all events correctly without duplicated events" in {
    val eventsRaw = Source.fromResource("sample2.txt").getLines.toList
    val matchState = getNewMatchState
    eventsRaw.foreach { encodedEvent =>
      parse(encodedEvent).map(matchState.addEvent)
    }

    matchState.getAllEvents().length shouldBe 29
  }

}
