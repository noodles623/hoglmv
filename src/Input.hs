module Input where

import Prelude hiding (Left,Right)
import SDL
import qualified Mesh

data Input = None
           | Up
           | Down
           | Left
           | Right
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

-- getKey (KeyboardEventData _ Released _ _) = None
-- getKey (KeyboardEventData _ Pressed True _) = None
getKey (KeyboardEventData _ Released _ _) = None
--getKey (KeyboardEventData _ Pressed False _) = None
getKey (KeyboardEventData _ Pressed _ keysym) =
  case keysymKeycode keysym of
    SDL.KeycodeQ -> Quit
    _ -> None

checkInput :: IO Bool
checkInput =
  mkInput <$> pollEvent >>=
  \r -> return $ case r of
    Quit -> False
    _ ->    True

parseScanCode :: SDL.Scancode -> Mesh.MoveCam
parseScanCode input = case input of
  SDL.ScancodeUp -> Mesh.RUp
  SDL.ScancodeDown -> Mesh.RDown
  SDL.ScancodeLeft -> Mesh.RLeft
  SDL.ScancodeRight -> Mesh.RRight
  SDL.ScancodePageUp -> Mesh.Forward
  SDL.ScancodePageDown -> Mesh.Back
  SDL.ScancodeW -> Mesh.TUp
  SDL.ScancodeS -> Mesh.TDown
  SDL.ScancodeD -> Mesh.TRight
  SDL.ScancodeA -> Mesh.TLeft
  _ -> Mesh.None

keys :: IO [SDL.Scancode]
keys = do
  held <- SDL.getKeyboardState
  return $ filter held
    [ SDL.ScancodeUp
    , SDL.ScancodeDown
    , SDL.ScancodeLeft
    , SDL.ScancodeRight
    , SDL.ScancodeW
    , SDL.ScancodeS
    , SDL.ScancodeD
    , SDL.ScancodeA
    , SDL.ScancodePageUp
    , SDL.ScancodePageDown ]

heldKeys :: IO [Mesh.MoveCam]
heldKeys = do
  heldKeys <- keys
  return $ map parseScanCode heldKeys
  

