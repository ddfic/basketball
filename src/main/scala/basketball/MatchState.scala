package basketball

import java.util.concurrent.atomic.AtomicInteger

import basketball.sanitization.{EventSanitization, EventSet}

import scala.collection.concurrent.TrieMap

/**
  *
  * @param strictSanitization If set to true all returned event sets will be consistent.
  *                           If set to false there may be some inconsistencies i.e missing events.
  */
class MatchState(strictSanitization: Boolean = true) extends EventSanitization {
  private val events: TrieMap[Int, MatchEvent] = TrieMap.empty
  private val pendingEvents: TrieMap[Int, MatchEvent] = TrieMap.empty
  private val lastEventTime: AtomicInteger = new AtomicInteger(0)

  def addEvent(event: MatchEvent): Unit = {
    sanitize(event::getPendingEvents, getLastEvent, strictSanitization) match {
      case EventSet(sanitized, pending, _) => {
        setPendingEvents(pending)
        sanitized.foreach { sanitizedEvent =>
          if(sanitizedEvent.elapsedMatchTime > lastEventTime.get) {
            lastEventTime.set(sanitizedEvent.elapsedMatchTime)
          }
          events.put(sanitizedEvent.elapsedMatchTime, sanitizedEvent)
        }
      }
    }
  }

  def getLastEvent(): Option[MatchEvent] = events.get(lastEventTime.get)

  def getLastEvents(numberOfEvents: Int): List[MatchEvent] = events.values.toList.sortBy(_.elapsedMatchTime).takeRight(numberOfEvents)

  def getAllEvents(): List[MatchEvent] = events.values.toList

  private def getPendingEvents() = pendingEvents.values.toList

  private def setPendingEvents(newEvents: List[MatchEvent]) = {
    pendingEvents.clear()
    newEvents.foreach(e => pendingEvents.put(e.elapsedMatchTime, e))
  }
}
