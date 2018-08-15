package basketball.sanitization

import basketball.MatchEvent

import scala.annotation.tailrec

trait EventSetSanitization {
  def sanitize(pendingEvents: List[MatchEvent], lastEvent: Option[MatchEvent]): EventSet = {
    if(lastEvent.isEmpty) {
      if(pendingEvents.headOption.map(isInitialEvent).getOrElse(false)) {
        sanitizeSet(EventSet(List(pendingEvents.head), pendingEvents.tail))
      }
      else {
        EventSet(lastEvent.toList, pendingEvents)
      }
    }
    else {
      sanitizeSet(EventSet(lastEvent.toList.sortBy(_.elapsedMatchTime), pendingEvents))
    }
  }

  @tailrec
  private def sanitizeSet(eventSet: EventSet): EventSet = {
    if(eventSet.sanitizationStatus != EventValidationResult.VALID || eventSet.pending.isEmpty || eventSet.sanitized.isEmpty) {
      eventSet
    }
    else {
      val currentEvent = eventSet.pending.head
      val lastEvent = eventSet.sanitized.head
      val result =
        if(currentEvent.elapsedMatchTime <= lastEvent.elapsedMatchTime) {
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
          handleValidationResults(validationResults, eventSet)
        }
      sanitizeSet(result)
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

  private def handleValidationResults(validationResult: EventValidationResult.Value, eventSet: EventSet) = {
    validationResult match {
      case EventValidationResult.VALID =>
        eventSet.copy(eventSet.pending.head::eventSet.sanitized, eventSet.pending.tail)
      case EventValidationResult.ONE_MISSING =>
        val isMalformed = (for {
          current <- eventSet.pending.headOption
          next <- eventSet.pending.tail.headOption
        } yield current.team1PointsTotal > next.team1PointsTotal || current.team2PointsTotal > next.team2PointsTotal).getOrElse(false)
        eventSet.copy(sanitizationStatus = if(isMalformed) EventValidationResult.ONE_NUMBER_MALFORMED else EventValidationResult.ONE_MISSING)
      case _ =>
        eventSet.copy(sanitizationStatus = validationResult)
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
    if (currentTeam1PointsTotal == previousTeam1PointsTotal + pointsScored && currentTeam2PointsTotal == previousTeam2PointsTotal) {
      EventValidationResult.VALID
    }
    else {
      val team1Diff = currentTeam1PointsTotal - previousTeam1PointsTotal - pointsScored
      val team2Diff = currentTeam2PointsTotal - previousTeam2PointsTotal
      if(team1Diff == 0 && Set(1, 2, 3).contains(team2Diff) ||
        team2Diff == 0 && Set(1, 2, 3).contains(team1Diff)) {
        EventValidationResult.ONE_MISSING
      }
      else if(team1Diff == 0 || team2Diff == 0) {
        EventValidationResult.ONE_NUMBER_MALFORMED
      }
      else {
        EventValidationResult.INVALID
      }
    }
  }
}
