module Key where

import World

keyName = "key"
key = MkItem { getUse = Nothing
              , getPut = True
              , getTake = True
              , getDisplay = "a key"
              }