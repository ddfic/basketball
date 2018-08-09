package basketball

import scala.util.Try

// TODO consts
object Parser {
  val LAST_BIT_INDEX = 31
  val POINTS_SCORED_START = 0
  val POINTS_SCORED_END = 1
  val WHO_SCORED_START = 2
  val WHO_SCORED_END = 2
  val TEAM_2_POINTS_TOTAL_START = 3
  val TEAM_2_POINTS_TOTAL_END = 10
  val TEAM_1_POINTS_TOTAL_START = 11
  val TEAM_1_POINTS_TOTAL_END = 18
  val ELAPSED_MATCH_TIME_START = 19
  val ELAPSED_MATCH_TIME_END = 30
}

trait Parser {
  import Parser._
  def parse(encodedEvent: String): Option[MatchEvent] = {
    hex2Int(encodedEvent).map(int2Event)
  }

  private def hex2Int(hex: String): Option[Int] = {
    Try(Integer.parseInt(hex.replaceAll("0x", ""), 16)).toOption
  }

  private def clearLeadingBits(numberOfBits: Int)(buffer: Int) =(buffer << numberOfBits) >>> numberOfBits

  private def clearTrailingBits(numberOfBits: Int)(buffer: Int) = buffer >>> numberOfBits

  private def readBits(start: Int, end: Int)= clearLeadingBits(LAST_BIT_INDEX - end)_ andThen clearTrailingBits(start)_

  private def int2Event(event: Int): MatchEvent = {
    val pointsScored = readBits(POINTS_SCORED_START, POINTS_SCORED_END)(event)
    val whoScored = readBits(WHO_SCORED_START, WHO_SCORED_END)(event)
    val team2PointsTotal = readBits(TEAM_2_POINTS_TOTAL_START, TEAM_2_POINTS_TOTAL_END)(event)
    val team1PointsTotal = readBits(TEAM_1_POINTS_TOTAL_START, TEAM_1_POINTS_TOTAL_END)(event)
    val elapsedMatchTime = readBits(ELAPSED_MATCH_TIME_START, ELAPSED_MATCH_TIME_END)(event)
    MatchEvent(pointsScored, whoScored, team1PointsTotal, team2PointsTotal, elapsedMatchTime)
  }
}
