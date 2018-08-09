package basketball.sanitization

import basketball.MatchEvent

import scala.annotation.tailrec

trait EventSanitization {
  def sanitize(pendingEvents: List[MatchEvent], lastEvent: Option[MatchEvent], strictSanitization: Boolean): EventSet = {
    if(lastEvent.isEmpty) {
      if(pendingEvents.headOption.map(isInitialEvent).getOrElse(false)) {
        sanitizeSet(EventSet(List(pendingEvents.head), pendingEvents.tail), strictSanitization)
      }
      else {
        EventSet(lastEvent.toList, pendingEvents)
      }
    }
    else {
      sanitizeSet(EventSet(lastEvent.toList.sortBy(_.elapsedMatchTime), pendingEvents), strictSanitization)
    }
  }

  @tailrec
  private def sanitizeSet(eventSet: EventSet, strictSanitization: Boolean): EventSet = {
    if(eventSet.missingEvent || eventSet.pending.isEmpty || eventSet.sanitized.isEmpty) {
      eventSet
    }
    else {
      val currentEvent = eventSet.pending.head
      val lastEvent = eventSet.sanitized.head
      val result =
        if(currentEvent.elapsedMatchTime < lastEvent.elapsedMatchTime) {
          // removing event because it's introducing inconsistency to already consistent set
          eventSet.copy(pending = eventSet.pending.tail)
        }
        else if(
          currentEvent.pointsScored == 0 &&
          currentEvent.team1PointsTotal == lastEvent.team1PointsTotal &&
          currentEvent.team2PointsTotal == lastEvent.team2PointsTotal
        ) {
          // removing event because it's not carrying any information
          eventSet.copy(pending = eventSet.pending.tail)
        }
        else {
          val validationResults = validateEvent(currentEvent, lastEvent)
          handleValidationResults(validationResults, eventSet, strictSanitization)
        }
      sanitizeSet(result, strictSanitization)
    }
  }

  private def isInitialEvent(event: MatchEvent) = {
    if(event.whoScored == 0) {
      event.pointsScored == event.team1PointsTotal && event.team2PointsTotal == 0
    }
    else {
      event.pointsScored == event.team2PointsTotal && event.team1PointsTotal == 0
    }
  }

  private def handleValidationResults(validationResult: EventValidationResult.Value, eventSet: EventSet, strictSanitization: Boolean) = {
    validationResult match {
      case EventValidationResult.VALID =>
        eventSet.copy(eventSet.pending.head::eventSet.sanitized, eventSet.pending.tail)
      case EventValidationResult.NON_CONSECUTIVE =>
        if(strictSanitization) {
          eventSet.copy(missingEvent = true)
        }
        else {
          eventSet.copy(eventSet.pending.head::eventSet.sanitized, eventSet.pending.tail)
        }
      case EventValidationResult.MALFORMED => eventSet.copy(missingEvent = true) // TODO handle malformed events
    }
  }

  private def validateEvent(currentEvent: MatchEvent, lastEvent: MatchEvent) = {
    if (currentEvent.whoScored == 0) {
      validateScores(currentEvent.pointsScored, currentEvent.team1PointsTotal, currentEvent.team2PointsTotal,
        lastEvent.team1PointsTotal, lastEvent.team2PointsTotal)
    }
    else {
      validateScores(currentEvent.pointsScored, currentEvent.team2PointsTotal, currentEvent.team1PointsTotal,
        lastEvent.team2PointsTotal, lastEvent.team1PointsTotal)
    }
  }

  private def validateScores(
                      pointsScored: Int,
                      currentTeam1PointsTotal: Int,
                      currentTeam2PointsTotal: Int,
                      previousTeam1PointsTotal: Int,
                      previousTeam2PointsTotal: Int) = {
    // TODO: detect malformed events
    if (currentTeam1PointsTotal == previousTeam1PointsTotal + pointsScored) {
      if (currentTeam2PointsTotal == previousTeam2PointsTotal) {
        EventValidationResult.VALID
      }
      else {
        EventValidationResult.NON_CONSECUTIVE
      }
    }
    else {
      EventValidationResult.NON_CONSECUTIVE
    }
  }
}
