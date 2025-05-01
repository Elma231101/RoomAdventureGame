module Kitchen where

import World
import RoomNames

kitchen :: Room
kitchen = MkRoom { getDescription = (Description "You are in the kitchen. There is some fastfood in the counter.")
                    , getRoomName = kitchenName
                    , getExits = [ (South, hallwayName)
                                 , (West, livingRoomName)]
                  }
