module Garden where

import World
import RoomNames

gardenRoom :: Room
gardenRoom = MkRoom { getDescription = (Description "You are in a beautiful garden. There is a well in front of you.")
                    , getRoomName = gardenName
                    , getExits = [ (West, hallwayName)]
                    }
