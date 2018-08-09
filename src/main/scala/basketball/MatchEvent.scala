package basketball

case class MatchEvent(
                       pointsScored: Int,
                       whoScored: Int,
                       team1PointsTotal: Int,
                       team2PointsTotal: Int,
                       elapsedMatchTime: Int
                     )
