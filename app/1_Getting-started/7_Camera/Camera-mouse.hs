{-# LANGUAGE RecursiveDo #-}
module Main where

import LOGL.Window
import LOGL.Texture
import Foreign.Ptr
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL as GL hiding (normalize)
import Graphics.GLUtil
import System.FilePath
import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects
import Linear.Matrix
import Linear.V3
import Linear.Vector
import Linear.Quaternion
import Linear.Projection
import Linear.Metric
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators hiding (empty)
import LOGL.FRP

cubePositions :: [V3 GLfloat]
cubePositions = [
    V3 0.0  0.0  0.0,
    V3 2.0  5.0 (-15.0),
    V3 (-1.5) (-2.2) (-2.5),
    V3 (-3.8) (-2.0) (-12.3),
    V3 2.4 (-0.4) (-3.5),
    V3 (-1.7) 3.0 (-7.5),
    V3 1.3 (-2.0) (-2.5),
    V3 1.5  2.0 (-2.5),
    V3 1.5  0.2 (-1.5),
    V3 (-1.3)  1.0 (-1.5)]

data Camera = Camera {  pos :: V3 GLfloat,
                        front :: V3 GLfloat,
                        up :: V3 GLfloat,
                        lastFrame :: Double,
                        lastX :: GLfloat,
                        lastY :: GLfloat,
                        yaw :: GLfloat,
                        pitch :: GLfloat,
                        firstMouse :: Bool}
    deriving (Eq, Show)

main :: IO ()
main = do
    GLFW.init
    w <- createAppWindow 800 600 "LearnOpenGL"

    setCursorInputMode (window w) CursorInputMode'Disabled

    depthFunc $= Just Less

    shader <- simpleShaderProgram ("data" </> "1_Getting-started" </> "6_Coordinate-systems" </> "coord-systems.vs")
        ("data" </> "1_Getting-started" </> "6_Coordinate-systems" </> "coord-systems.frag")
    (vao, vbo) <- createVAO

    -- load and create texture
    t0 <- createTexture ("data" </> "1_Getting-started" </> "4_Textures" </> "Textures" </> "container.jpg")
    t1 <- createTexture ("data" </> "1_Getting-started" </> "4_Textures" </> "Textures-combined" </> "awesomeface3.png")

    -- init camera
    let initCam = Camera { pos = V3 0.0 0.0 3.0, front = V3 0.0 0.0 (-1.0) , up = V3 0.0 1.0 0.0,
                            lastFrame = 0.0, lastX = 400.0, lastY = 300.0, yaw = -90.0,
                            pitch = 0.0, firstMouse = True}

    --polygonMode $= (Line, Line)
    let networkDescription :: MomentIO ()
        networkDescription = mdo
            posE <- cursorPosEvent w
            idleE <- idleEvent w
            timeB <- currentTimeB
            keyB <- keyBehavior w
            camB <- accumB initCam $ unions [
                        handlePosEvent <$> posE,
                        (doMovement <$> keyB ) <@> (timeB <@ idleE)]
            reactimate $ drawScene shader t0 t1 vao w <$> (camB <@ idleE)
    runAppLoopEx w networkDescription

    deleteObjectName vao
    deleteObjectName vbo
    terminate

handlePosEvent :: CursorPosEvent -> Camera -> Camera
handlePosEvent (w, xpos, ypos) cam = cam {lastX = realToFrac xpos, lastY = realToFrac ypos,
                                        yaw = newYaw, pitch = newPitch,
                                        front = normalize (V3 newFrontX newFrontY newFrontZ),
                                        firstMouse = False}
    where
        lx = if firstMouse cam then realToFrac xpos else lastX cam
        ly = if firstMouse cam then realToFrac ypos else lastY cam
        sensivity = 0.5
        xoffset = ( realToFrac xpos - lx) * sensivity
        yoffset = (ly - realToFrac ypos) * sensivity
        newYaw = yaw cam + xoffset
        newPitch = restrictPitch $ pitch cam + yoffset
        newFrontX = cos (radians newYaw) * cos (radians newPitch)
        newFrontY = sin (radians newPitch)
        newFrontZ = sin (radians newYaw) * cos (radians newPitch)

radians :: GLfloat -> GLfloat
radians deg = pi / 180.0 * deg

restrictPitch :: GLfloat -> GLfloat
restrictPitch p
    | p > 89.0 = 89.0
    | p < (-89.0) = -89.0
    | otherwise = p

doMovement :: Keys -> Double -> Camera -> Camera
doMovement keys time cam = afterMoveRight {lastFrame = time}
    where
        speed =   5.0 * realToFrac (time - lastFrame cam)
        upPressed = keyPressed Key'W keys
        downPressed = keyPressed Key'S keys
        leftPressed = keyPressed Key'A keys
        rightPressed = keyPressed Key'D keys
        afterZoomIn = if upPressed then moveForeward speed cam else cam
        afterZoomOut = if downPressed then moveBackward speed afterZoomIn else afterZoomIn
        afterMoveLeft = if leftPressed then moveLeft speed afterZoomOut else afterZoomOut
        afterMoveRight = if rightPressed then moveRight speed afterMoveLeft else afterMoveLeft

moveForeward :: GLfloat -> Camera -> Camera
moveForeward speed cam = cam { pos = pos cam ^+^ (speed *^ front cam) }

moveBackward :: GLfloat -> Camera -> Camera
moveBackward speed cam = cam { pos = pos cam ^-^ (speed *^ front cam) }

moveLeft :: GLfloat -> Camera -> Camera
moveLeft speed cam = cam { pos = pos cam ^-^ (speed *^ normalize (cross (front cam ) (up cam)))}

moveRight :: GLfloat -> Camera -> Camera
moveRight speed cam = cam { pos = pos cam ^+^ (speed *^ normalize (cross (front cam ) (up cam)))}

drawScene :: ShaderProgram -> TextureObject -> TextureObject -> VertexArrayObject -> AppWindow -> Camera -> IO ()
drawScene shader t0 t1 vao w cam = do
    pollEvents
    clearColor $= Color4 0.2 0.3 0.3 1.0
    clear [ColorBuffer, DepthBuffer]

    -- Draw our first triangle
    currentProgram $= Just (program shader)

    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just t0
    setUniform shader "ourTexture1" (TextureUnit 0)

    activeTexture $= TextureUnit 1
    textureBinding Texture2D $= Just t1
    setUniform shader "ourTexture2" (TextureUnit 1)

    let view = lookAt (pos cam) (pos cam + front cam) (up cam)
        projection = perspective (pi / 4.0) (800.0 / 600.0) 0.1 (100.0 :: GLfloat)
    setUniform shader "view" view
    setUniform shader "projection" projection

    withVAO vao $ mapM_ (drawCube shader) [0..9]
    swap w

drawCube :: ShaderProgram -> Int -> IO ()
drawCube shader i = do
    let angle = pi / 180.0 * 20.0 * fromIntegral i
        rot = axisAngle (V3 (1.0 :: GLfloat) 0.3 0.5) (realToFrac angle)
        model = mkTransformation rot (cubePositions !! i)
    setUniform shader "model" model
    drawArrays Triangles 0 36

createVAO :: IO (VertexArrayObject, BufferObject)
createVAO = do
    vao <- genObjectName
    bindVertexArrayObject $= Just vao
    vbo <- makeBuffer ArrayBuffer cubeWithTexture
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (5*4) offset0)
    vertexAttribArray (AttribLocation 0) $= Enabled
    vertexAttribPointer (AttribLocation 2) $= (ToFloat, VertexArrayDescriptor 2 Float (5*4) (offsetPtr (3*4)))
    vertexAttribArray (AttribLocation 2) $= Enabled
    bindVertexArrayObject $= Nothing
    return (vao, vbo)
