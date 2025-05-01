module BedRoom where

import World
import RoomNames

bedRoom :: Room
bedRoom = MkRoom { getDescription = (Description "You are in the bedroom. Take a demetrin and get ready to sleep! zZzZz")
                    , getRoomName = bedRoomName
                    , getExits = [ (East, hallwayName)]
                    }
