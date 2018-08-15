package basketball.sanitization

import basketball.MatchEvent

trait EventSetHealing extends EventSetSanitization {
  def getAllEventsRelaxed(status: EventValidationResult.Value, pending: List[MatchEvent], sanitized: List[MatchEvent]): List[MatchEvent] = {
    if(pending == Nil) {
      sanitized
    }
    else if(status == EventValidationResult.ONE_MISSING) {
      val sanitizationResult = sanitize(pending.tail, pending.headOption)
      getAllEventsRelaxed(sanitizationResult.sanitizationStatus, sanitizationResult.pending, sanitizationResult.sanitized ++ sanitized)
    }
    else if(status == EventValidationResult.ONE_NUMBER_MALFORMED && pending.tail.nonEmpty) {
      val nextEvent = pending.tail.head
      val fixedEvent = {
        pending.head.copy(
          team1PointsTotal = nextEvent.team1PointsTotal - (if(nextEvent.whoScored == 0) nextEvent.pointsScored else 0),
          team2PointsTotal = nextEvent.team2PointsTotal - (if(nextEvent.whoScored == 1) nextEvent.pointsScored else 0)
        )
      }
      val sanitizationResult = sanitize(pending.tail, Some(fixedEvent))
      getAllEventsRelaxed(sanitizationResult.sanitizationStatus, sanitizationResult.pending, sanitizationResult.sanitized ++ sanitized)
    }
    else {
      sanitized
    }
  }
}
