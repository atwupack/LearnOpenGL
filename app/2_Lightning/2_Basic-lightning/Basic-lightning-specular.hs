{-# LANGUAGE RecursiveDo #-}
module Main where

import LOGL.Window
import LOGL.Texture
import LOGL.Camera
import Foreign.Ptr
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL as GL hiding (normalize, position)
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
import LOGL.Objects

data AppState = AppState { camera :: Camera GLfloat,
                        lastFrame :: Double,
                        lastX :: GLfloat,
                        lastY :: GLfloat,
                        firstMouse :: Bool}
    deriving (Show)

main :: IO ()
main = do
    GLFW.init
    w <- createAppWindow 800 600 "LearnOpenGL"

    setCursorInputMode (window w) CursorInputMode'Disabled

    depthFunc $= Just Less

    lampShader <- simpleShaderProgram ("data" </> "2_Lightning" </> "1_Colors" </> "lamp.vs")
        ("data" </> "2_Lightning" </> "1_Colors" </> "lamp.frag")

    lightningShader <- simpleShaderProgram ("data" </> "2_Lightning" </> "2_Basic-lightning" </> "specular.vs")
        ("data" </> "2_Lightning" </> "2_Basic-lightning" </> "specular.frag")

    cubeVBO <- createCubeVBO
    containerVAO <- createContVAO cubeVBO
    lightVAO <- createLampVAO cubeVBO

    let initState = AppState {  camera = createCamera (V3 0.0 0.0 3.0) (V3 0.0 1.0 0.0) (-90.0) 0.0,
                                firstMouse = True,
                                lastX = 400.0,
                                lastY = 300.0,
                                lastFrame = 0.0}

    --polygonMode $= (Line, Line)
    let networkDescription :: MomentIO ()
        networkDescription = mdo
            posE <- cursorPosEvent w
            scrollE <- scrollEvent w
            idleE <- idleEvent w
            timeB <- currentTimeB
            keyB <- keyBehavior w
            stateB <- accumB initState $ unions [
                        handleScrollEvent <$> scrollE,
                        handlePosEvent <$> posE,
                        (doMovement <$> keyB ) <@> (timeB <@ idleE)]
            reactimate $ drawScene lightningShader containerVAO lampShader lightVAO w <$> (stateB <@ idleE)
    runAppLoopEx w networkDescription

    deleteObjectName lightVAO
    deleteObjectName containerVAO
    deleteObjectName cubeVBO
    terminate

handleScrollEvent :: ScrollEvent -> AppState -> AppState
handleScrollEvent (w, xoffset, yoffset) state = state { camera = processMouseScroll (camera state) (realToFrac yoffset) }

handlePosEvent :: CursorPosEvent -> AppState -> AppState
handlePosEvent (w, xpos, ypos) state = state { lastX = realToFrac xpos, lastY = realToFrac ypos, firstMouse = False,
                                                camera = processMouseMovement cam xoffset yoffset True }
    where
        cam = camera state
        lx = if firstMouse state then realToFrac xpos else lastX state
        ly = if firstMouse state then realToFrac ypos else lastY state
        xoffset =  realToFrac xpos - lx
        yoffset = ly - realToFrac ypos

doMovement :: Keys -> Double -> AppState -> AppState
doMovement keys time state = state { camera = afterMoveRight , lastFrame = time}
    where
        cam = camera state
        deltaTime = realToFrac $ time - lastFrame state
        upPressed = keyPressed Key'W keys
        downPressed = keyPressed Key'S keys
        leftPressed = keyPressed Key'A keys
        rightPressed = keyPressed Key'D keys
        afterZoomIn = if upPressed then processKeyboard cam ForwardM deltaTime else cam
        afterZoomOut = if downPressed then processKeyboard afterZoomIn BackwardM  deltaTime else afterZoomIn
        afterMoveLeft = if leftPressed then processKeyboard afterZoomOut LeftM  deltaTime else afterZoomOut
        afterMoveRight = if rightPressed then processKeyboard afterMoveLeft RightM  deltaTime else afterMoveLeft

drawScene :: ShaderProgram -> VertexArrayObject -> ShaderProgram -> VertexArrayObject -> AppWindow -> AppState -> IO ()
drawScene lightningShader contVAO lampShader lightVAO w state = do
    pollEvents
    clearColor $= Color4 0.1 0.1 0.1 1.0
    clear [ColorBuffer, DepthBuffer]

    Just time <- getTime

    let lightX = 1.0 + 2.0 * sin time
        lightY = sin (time / 2.0)
        lightPos = V3 (realToFrac  lightX) (realToFrac lightY) (2.0 :: GLfloat)
        cam = camera state

    -- draw the container cube
    currentProgram $= Just (program lightningShader)
    setUniform lightningShader "objectColor" (V3 (1.0 :: GLfloat) 0.5 0.31)
    setUniform lightningShader "lightColor" (V3 (1.0 :: GLfloat) 1.0 1.0)
    setUniform lightningShader "lightPos" lightPos
    setUniform lightningShader "viewPos" (position cam)

    let view = viewMatrix cam
        projection = perspective (radians (zoom cam)) (800.0 / 600.0) 0.1 (100.0 :: GLfloat)
    setUniform lightningShader "view" view
    setUniform lightningShader "projection" projection

    let model = identity :: M44 GLfloat
    setUniform lightningShader "model" model
    withVAO contVAO $ drawArrays Triangles 0 36

    -- draw the lamp
    currentProgram $= Just (program lampShader)
    setUniform lampShader "view" view
    setUniform lampShader "projection" projection

    let model = mkTransformationMat (0.2 *!! identity) lightPos
    setUniform lampShader "model" model
    withVAO lightVAO $ drawArrays Triangles 0 36

    swap w

createCubeVBO :: IO BufferObject
createCubeVBO = makeBuffer ArrayBuffer cubeWithNormals

createContVAO :: BufferObject -> IO VertexArrayObject
createContVAO vbo = do
    vao <- genObjectName
    bindVertexArrayObject $= Just vao
    bindBuffer ArrayBuffer $= Just vbo
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (6*4) offset0)
    vertexAttribArray (AttribLocation 0) $= Enabled
    vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 3 Float (6*4) (offsetPtr (3*4)))
    vertexAttribArray (AttribLocation 1) $= Enabled
    bindVertexArrayObject $= Nothing
    return vao

createLampVAO :: BufferObject -> IO VertexArrayObject
createLampVAO vbo = do
    vao <- genObjectName
    bindVertexArrayObject $= Just vao
    bindBuffer ArrayBuffer $= Just vbo
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (6*4) offset0)
    vertexAttribArray (AttribLocation 0) $= Enabled
    bindVertexArrayObject $= Nothing
    return vao
