data MyBool = False | True

data Direction = North | East | South | West

turnClockWise : Direction -> Direction
turnClockWise North = East
turnClockWise East = South
turnClockWise South = West
turnClockWise West = North
