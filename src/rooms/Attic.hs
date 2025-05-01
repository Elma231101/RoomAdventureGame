module Attic where

import World
import RoomNames

atticRoom :: Room
atticRoom = MkRoom { getDescription = (Description "You are in the attic. There is a giant welding torch in the corner.")
                     , getRoomName = atticName
                     , getExits = [ (Down, hallwayName)]
                     }
