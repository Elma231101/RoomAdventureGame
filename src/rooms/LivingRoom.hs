module LivingRoom where

import World
import RoomNames

livingRoom :: Room
livingRoom = MkRoom { getDescription = (Description "You are in the living-room. A wizard is snoring loudly on the couch.")
                    , getRoomName = livingRoomName
                    , getExits = [ (East, kitchenName)
                                 , (SouthEast, hallwayName)]
                    }
