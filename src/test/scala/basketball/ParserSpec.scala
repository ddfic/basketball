package basketball

import org.scalatest.{FlatSpecLike, Matchers}


class ParserSpec extends FlatSpecLike with Matchers with Parser {

  it should "Parse event: after 15 seconds of play, Team 1 scores 2 points" in {
    val result = parse("0x781002")
    result.nonEmpty shouldBe true
    result.get.pointsScored shouldBe 2
    result.get.whoScored shouldBe 0
    result.get.team1PointsTotal shouldBe 2
    result.get.team2PointsTotal shouldBe 0
    result.get.elapsedMatchTime shouldBe 15
  }

  it should "Parse event: 15 seconds later, Team 2 replies with 3 points" in {
    val result = parse("0xf0101f")
    result.nonEmpty shouldBe true
    result.get.pointsScored shouldBe 3
    result.get.whoScored shouldBe 1
    result.get.team1PointsTotal shouldBe 2
    result.get.team2PointsTotal shouldBe 3
    result.get.elapsedMatchTime shouldBe 30
  }

  it should "Parse event: At 10:10, a single point for Team 1 gives them a 5 point lead - 25-20" in {
    val result = parse("0x1310c8a1")
    result.nonEmpty shouldBe true
    result.get.pointsScored shouldBe 1
    result.get.whoScored shouldBe 0
    result.get.team1PointsTotal shouldBe 25
    result.get.team2PointsTotal shouldBe 20
    result.get.elapsedMatchTime shouldBe 610
  }

  it should "Parse event: At 22:23, a 2-point shot for Team 1 leaves them 4 points behind at 48-52" in {
    val result = parse("0x29f981a2")
    result.nonEmpty shouldBe true
    result.get.pointsScored shouldBe 2
    result.get.whoScored shouldBe 0
    result.get.team1PointsTotal shouldBe 48
    result.get.team2PointsTotal shouldBe 52
    result.get.elapsedMatchTime shouldBe 1343
  }

  it should "Parse event: At 38:30, a 3-point shot levels the game for Team 2 at 100 points each" in {
    val result = parse("0x48332327")
    result.nonEmpty shouldBe true
    result.get.pointsScored shouldBe 3
    result.get.whoScored shouldBe 1
    result.get.team1PointsTotal shouldBe 100
    result.get.team2PointsTotal shouldBe 100
    result.get.elapsedMatchTime shouldBe 2310
  }

  it should "Return None when the string is empty" in {
    val result = parse("")
    result shouldBe None
  }

}
