module Input where

import Prelude
import SDL

data Input = None
           | Quit
  deriving (Eq, Show)

mkInput :: Maybe Event -> Input
mkInput = maybe None (payloadToInput . extractPayload)

extractPayload :: Event -> EventPayload
extractPayload (Event _t p) = p

payloadToInput :: EventPayload -> Input
payloadToInput QuitEvent         = Quit
payloadToInput (KeyboardEvent k) = getKey k
payloadToInput _                 = None

getKey (KeyboardEventData _ Released _ _) = None
getKey (KeyboardEventData _ Pressed True _) = None
getKey (KeyboardEventData _ Pressed False keysym) =
  case keysymKeycode keysym of
    KeycodeQ -> Quit
    _ -> None

checkInput :: IO Bool
checkInput =
  mkInput <$> pollEvent >>=
  \ x -> return $ case x of Quit -> False
                            _ -> True
