package basketball.sanitization

import basketball.MatchEvent

case class EventSet(sanitized: List[MatchEvent], pending: List[MatchEvent],
                    sanitizationStatus: EventValidationResult.Value = EventValidationResult.VALID)
