package basketball.sanitization

import basketball.{MatchEvent, MatchState, Parser}
import org.scalatest.{FlatSpecLike, Matchers}

import scala.io.Source


class SanitizationSpec extends FlatSpecLike with Matchers with Parser {

  it should "Do strict sanitization correctly" in {
    val eventsRaw = Source.fromResource("sample2.txt").getLines.toList
    val matchState = new MatchState
    eventsRaw.foreach { encodedEvent =>
      parse(encodedEvent).map(matchState.addEvent)
    }

    matchState.getAllEvents.sortBy(_.elapsedMatchTime) shouldBe List(
      MatchEvent(2,0,2,0,15),
      MatchEvent(2,1,2,2,28),
      MatchEvent(3,1,2,5,60),
      MatchEvent(2,0,4,5,75),
      MatchEvent(2,1,4,7,97),
      MatchEvent(2,1,4,9,113),
      MatchEvent(3,0,7,9,122),
      MatchEvent(2,1,7,11,143),
      MatchEvent(1,0,8,11,168),
      MatchEvent(2,1,8,13,195),
      MatchEvent(2,0,10,13,215),
      MatchEvent(2,1,10,15,251),
      MatchEvent(2,0,12,15,279),
      MatchEvent(2,1,12,17,295),
      MatchEvent(2,1,12,19,322)
    )
  }

  it should "Do relaxed sanitization correctly" in {
    val eventsRaw = Source.fromResource("sample2.txt").getLines.toList
    val matchState = new MatchState(false)
    eventsRaw.foreach { encodedEvent =>
      parse(encodedEvent).map(matchState.addEvent)
    }

    matchState.getAllEvents().sortBy(_.elapsedMatchTime).foreach(x => {
      println(s"$x")
    })

    matchState.getAllEvents.sortBy(_.elapsedMatchTime) shouldBe List(
      MatchEvent(2,0,2,0,15),
      MatchEvent(2,1,2,2,28),
      MatchEvent(3,1,2,5,60),
      MatchEvent(2,0,4,5,75),
      MatchEvent(2,1,4,7,97),
      MatchEvent(2,1,4,9,113),
      MatchEvent(3,0,7,9,122),
      MatchEvent(2,1,7,11,143),
      MatchEvent(1,0,8,11,168),
      MatchEvent(2,1,8,13,195),
      MatchEvent(2,0,10,13,215),
      MatchEvent(2,1,10,15,251),
      MatchEvent(2,0,12,15,279),
      MatchEvent(2,1,12,17,295),
      MatchEvent(2,1,12,19,322),
      MatchEvent(2,0,14,20,369),
      MatchEvent(2,1,14,22,393),
      MatchEvent(2,1,14,24,408),
      MatchEvent(2,0,16,24,426),
      MatchEvent(2,1,16,26,446),
      MatchEvent(3,0,19,26,456),
      MatchEvent(2,1,19,28,472),
      MatchEvent(2,0,21,28,505),
      MatchEvent(1,1,21,29,533),
      MatchEvent(2,0,23,29,560),
      MatchEvent(2,0,25,29,579)
    )
  }

  // TODO: more tests

}
