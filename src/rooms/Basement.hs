module Basement where

import World
import RoomNames

basement :: Room
basement = MkRoom { getDescription = (Description "You are in the basement. There are tools for everything here.")
                    , getRoomName =  basementName
                    , getExits = [ (Up, hallwayName)
                                 ]
                  }
