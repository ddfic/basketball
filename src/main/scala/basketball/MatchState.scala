package basketball

import java.util.concurrent.atomic.AtomicInteger

import basketball.sanitization.{EventSet, EventSetHealing, EventSetSanitization}

import scala.collection.concurrent.TrieMap

/**
  *
  * @param strictSanitization If set to true all returned event sets will be consistent.
  *                           If set to false there may be some inconsistencies i.e missing events.
  */
class MatchState(strictSanitization: Boolean = true) extends EventSetSanitization with EventSetHealing {
  private val events: TrieMap[Int, MatchEvent] = TrieMap.empty
  private val pendingEvents: TrieMap[Int, MatchEvent] = TrieMap.empty
  private val lastEventTime: AtomicInteger = new AtomicInteger(0)

  def addEvent(event: MatchEvent): Unit = {
    sanitize(event::getPendingEvents, getLastEvent) match {
      case EventSet(sanitized, pending, status) => {
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

  def getLastEvents(numberOfEvents: Int): List[MatchEvent] = getAllEvents().sortBy(_.elapsedMatchTime).takeRight(numberOfEvents)

  def getAllEvents(): List[MatchEvent] = {
    if(strictSanitization) {
      events.values.toList
    }
    else {
      val sanitizationResult = sanitize(getPendingEvents, getLastEvent)
      getAllEventsRelaxed(sanitizationResult.sanitizationStatus, getPendingEvents, events.values.toList.sortBy(_.elapsedMatchTime))
    }
  }

  private def getPendingEvents() = pendingEvents.values.toList.sortBy(_.elapsedMatchTime)

  private def setPendingEvents(newEvents: List[MatchEvent]) = {
    pendingEvents.clear()
    newEvents.foreach(e => pendingEvents.put(e.elapsedMatchTime, e))
  }
}
