module LOGL.Camera
(
    Camera, CameraMovement(..), processKeyboard, processMouseScroll, processMouseMovement,
    viewMatrix, createCamera, zoom, radians
)
where

import Linear.V3
import Linear.Vector
import Linear.Metric
import Linear.Epsilon
import Linear.Matrix
import Linear.Projection
import Graphics.Rendering.OpenGL.GL as GL hiding (position, normalize)

data CameraMovement = ForwardM | BackwardM | LeftM | RightM

data Camera a = Camera {  position :: V3 a,
                        front :: V3 a,
                        up :: V3 a,
                        right :: V3 a,
                        worldUp :: V3 a,
                        yaw :: a,
                        pitch :: a,
                        movementSpeed :: a,
                        mouseSensitivity :: a,
                        zoom :: a}
    deriving (Show)

defaultCamera :: (RealFloat a, Epsilon a) => Camera a
defaultCamera = Camera { yaw = -90.0, pitch = 0.0 , movementSpeed = 3.0, mouseSensitivity = 0.25, zoom = 45.0,
                        front = V3 0.0 0.0 (-1.0), position = V3 0.0 0.0 0.0, up = V3 0.0 1.0 0.0,
                        right = V3 0.0 0.0 0.0, worldUp = V3 0.0 1.0 0.0}

createCamera :: (RealFloat a, Epsilon a) => V3 a -> V3 a -> a -> a -> Camera a
createCamera pos up y p = updateCameraVectors ( defaultCamera { position = pos, worldUp = up, yaw = y, pitch = p } )

viewMatrix :: (RealFloat a, Epsilon a) => Camera a -> M44 a
viewMatrix cam = lookAt (position cam) (position cam + front cam) (up cam)

processMouseMovement :: (RealFloat a, Epsilon a) => Camera a -> a -> a -> Bool -> Camera a
processMouseMovement cam xoffset yoffset constrainPitch =
    updateCameraVectors (cam {   yaw = yaw cam + xoffset * mouseSensitivity cam,
            pitch = restrictPitch constrainPitch (pitch cam + yoffset * mouseSensitivity cam) })

restrictPitch :: (RealFloat a) => Bool ->  a -> a
restrictPitch enabled p
    | not enabled = p
    | p > 89.0 = 89.0
    | p < (-89.0) = -89.0
    | otherwise = p

processKeyboard :: (RealFloat a) => Camera a -> CameraMovement -> a -> Camera a
processKeyboard cam dir deltaTime =
    case dir of
        ForwardM -> cam { position = position cam ^+^ (velocity *^ front cam) }
        BackwardM -> cam { position = position cam ^-^ (velocity *^ front cam) }
        LeftM -> cam { position = position cam ^-^ (velocity *^ right cam) }
        RightM -> cam { position = position cam ^+^ (velocity *^ right cam) }
    where
        velocity = movementSpeed cam * deltaTime

processMouseScroll :: (RealFloat a) => Camera a -> a -> Camera a
processMouseScroll cam yoffset = cam { zoom = restrictZoom newZoom}
    where
        newZoom = zoom cam - yoffset

restrictZoom :: (RealFloat a) => a -> a
restrictZoom z
    | z < 1.0 = 1.0
    | z > 45.0 = 45.0
    | otherwise = z

radians :: (RealFloat a) => a -> a
radians deg = pi / 180.0 * deg

updateCameraVectors :: (RealFloat a, Epsilon a) => Camera a -> Camera a
updateCameraVectors cam = cam { front = newFront, right = newRight, up = newUp}
    where
        newFrontX = cos (radians (yaw cam)) * cos (radians (pitch cam))
        newFrontY = sin (radians (pitch cam))
        newFrontZ = sin (radians (yaw cam)) * cos (radians (pitch cam))
        newFront = normalize ( V3 newFrontX newFrontY newFrontZ )
        newRight = normalize $ cross newFront (worldUp cam)
        newUp = normalize $ cross newRight newFront
