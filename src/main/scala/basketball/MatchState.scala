package basketball

import scala.collection.concurrent.TrieMap

class MatchState {
  private val events: TrieMap[Int, MatchEvent] = TrieMap.empty

  def addEvent(e: MatchEvent): Option[MatchEvent] = events.put(e.elapsedMatchTime, e)

  def getLastEvent(): Option[MatchEvent] = events.get(events.keySet.max)

  def getLastEvents(numberOfEvents: Int): List[MatchEvent] = events.values.toList.sortBy(_.elapsedMatchTime).takeRight(numberOfEvents)

  def getAllEvents(): List[MatchEvent] = events.values.toList
}
