package basketball

case class MatchEvent(
                       pointsScored: Int,
                       whoScored: Int, // TODO: change to enum
                       team1PointsTotal: Int,
                       team2PointsTotal: Int,
                       elapsedMatchTime: Int
                     )
