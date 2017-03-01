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
import Data.Set hiding (unions)

vertices :: [GLfloat]
vertices = [
        -0.5, -0.5, -0.5,  0.0, 0.0,
        0.5, -0.5, -0.5,  1.0, 0.0,
        0.5,  0.5, -0.5,  1.0, 1.0,
        0.5,  0.5, -0.5,  1.0, 1.0,
       -0.5,  0.5, -0.5,  0.0, 1.0,
       -0.5, -0.5, -0.5,  0.0, 0.0,

       -0.5, -0.5,  0.5,  0.0, 0.0,
        0.5, -0.5,  0.5,  1.0, 0.0,
        0.5,  0.5,  0.5,  1.0, 1.0,
        0.5,  0.5,  0.5,  1.0, 1.0,
       -0.5,  0.5,  0.5,  0.0, 1.0,
       -0.5, -0.5,  0.5,  0.0, 0.0,

       -0.5,  0.5,  0.5,  1.0, 0.0,
       -0.5,  0.5, -0.5,  1.0, 1.0,
       -0.5, -0.5, -0.5,  0.0, 1.0,
       -0.5, -0.5, -0.5,  0.0, 1.0,
       -0.5, -0.5,  0.5,  0.0, 0.0,
       -0.5,  0.5,  0.5,  1.0, 0.0,

        0.5,  0.5,  0.5,  1.0, 0.0,
        0.5,  0.5, -0.5,  1.0, 1.0,
        0.5, -0.5, -0.5,  0.0, 1.0,
        0.5, -0.5, -0.5,  0.0, 1.0,
        0.5, -0.5,  0.5,  0.0, 0.0,
        0.5,  0.5,  0.5,  1.0, 0.0,

       -0.5, -0.5, -0.5,  0.0, 1.0,
        0.5, -0.5, -0.5,  1.0, 1.0,
        0.5, -0.5,  0.5,  1.0, 0.0,
        0.5, -0.5,  0.5,  1.0, 0.0,
       -0.5, -0.5,  0.5,  0.0, 0.0,
       -0.5, -0.5, -0.5,  0.0, 1.0,

       -0.5,  0.5, -0.5,  0.0, 1.0,
        0.5,  0.5, -0.5,  1.0, 1.0,
        0.5,  0.5,  0.5,  1.0, 0.0,
        0.5,  0.5,  0.5,  1.0, 0.0,
       -0.5,  0.5,  0.5,  0.0, 0.0,
       -0.5,  0.5, -0.5,  0.0, 1.0]

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
                        speed :: GLfloat,
                        pressedKeys :: Set Key}
    deriving (Eq, Show)

main :: IO ()
main = do
    GLFW.init
    w <- createAppWindow 800 600 "LearnOpenGL"

    depthFunc $= Just Less

    shader <- simpleShaderProgram ("data" </> "1_Getting-started" </> "6_Coordinate-systems" </> "coord-systems.vs")
        ("data" </> "1_Getting-started" </> "6_Coordinate-systems" </> "coord-systems.frag")
    (vao, vbo) <- createVAO

    -- load and create texture
    t0 <- createTexture ("data" </> "1_Getting-started" </> "4_Textures" </> "Textures" </> "container.jpg")
    t1 <- createTexture ("data" </> "1_Getting-started" </> "4_Textures" </> "Textures-combined" </> "awesomeface3.png")

    -- init camera
    let initCam = Camera { pos = V3 0.0 0.0 3.0, front = V3 0.0 0.0 (-1.0) , up = V3 0.0 1.0 0.0, speed = 0.01,
                            pressedKeys = empty}

    --polygonMode $= (Line, Line)
    let networkDescription :: MomentIO ()
        networkDescription = mdo
            keyE <- keyEvent w
            idleE <- idleEvent w
            camB <- accumB initCam $ unions [
                        handleKeyEvent <$> keyE,
                        doMovement <$ idleE]
            reactimate $ drawScene shader t0 t1 vao w <$> (camB <@ idleE)
    runAppLoopEx w networkDescription

    deleteObjectName vao
    deleteObjectName vbo
    terminate

handleKeyEvent ::  KeyEvent -> Camera -> Camera
handleKeyEvent  (w, k, i, KeyState'Pressed, m) cam = cam { pressedKeys = insert k (pressedKeys cam ) }
handleKeyEvent  (w, k, i, KeyState'Released, m) cam = cam { pressedKeys = delete k (pressedKeys cam ) }
handleKeyEvent  (w, k, i, _, m) cam = cam

doMovement :: Camera -> Camera
doMovement cam = afterMoveRight
    where
        upPressed = member Key'W (pressedKeys cam)
        downPressed = member Key'S (pressedKeys cam)
        leftPressed = member Key'A (pressedKeys cam)
        rightPressed = member Key'D (pressedKeys cam)
        afterZoomIn = if upPressed then zoomIn cam else cam
        afterZoomOut = if downPressed then zoomOut afterZoomIn else afterZoomIn
        afterMoveLeft = if leftPressed then moveLeft afterZoomOut else afterZoomOut
        afterMoveRight = if rightPressed then moveRight afterMoveLeft else afterMoveLeft

zoomIn :: Camera -> Camera
zoomIn cam = cam { pos = pos cam ^+^ (speed cam *^ front cam) }

zoomOut :: Camera -> Camera
zoomOut cam = cam { pos = pos cam ^-^ (speed cam *^ front cam) }

moveLeft :: Camera -> Camera
moveLeft cam = cam { pos = pos cam ^-^ (speed cam *^ normalize (cross (front cam ) (up cam)))}

moveRight :: Camera -> Camera
moveRight cam = cam { pos = pos cam ^+^ (speed cam *^ normalize (cross (front cam ) (up cam)))}

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
    vbo <- makeBuffer ArrayBuffer vertices
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (5*4) offset0)
    vertexAttribArray (AttribLocation 0) $= Enabled
    vertexAttribPointer (AttribLocation 2) $= (ToFloat, VertexArrayDescriptor 2 Float (5*4) (offsetPtr (3*4)))
    vertexAttribArray (AttribLocation 2) $= Enabled
    bindVertexArrayObject $= Nothing
    return (vao, vbo)
