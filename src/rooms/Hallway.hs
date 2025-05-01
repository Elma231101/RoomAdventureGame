module Hallway where

import World
import RoomNames

hallway :: Room
hallway = MkRoom { getDescription = (Description "You are in the hallway. You have access to the entire house from here.")
                    , getRoomName = hallwayName
                    , getExits = [ (Up, atticName)
                                 , (East, gardenName)
                                 , (West, bedRoomName)
                                 , (North, kitchenName)
                                 , (NorthWest, livingRoomName)
                                 , (Down, basementName)]
                    }
