{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
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

main :: IO ()
main = do
    GLFW.init
    w <- createAppWindow 800 600 "LearnOpenGL"

    setCursorInputMode (window w) CursorInputMode'Disabled

    depthFunc $= Just Less

    lampShader <- simpleShaderProgram ("data" </> "2_Lighting" </> "1_Colors" </> "lamp.vs")
        ("data" </> "2_Lighting" </> "1_Colors" </> "lamp.frag")

    lightingShader <- simpleShaderProgram ("data" </> "2_Lighting" </> "5_Light-casters" </> "point.vs")
        ("data" </> "2_Lighting" </> "5_Light-casters" </> "point.frag")

    diffuseMap <- createTexture ("data" </> "2_Lighting" </> "4_Lighting-maps" </> "container2.png")
    specularMap <- createTexture ("data" </> "2_Lighting" </> "4_Lighting-maps" </> "container2_specular.png")

    currentProgram $= Just (program lightingShader)
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just diffuseMap
    setUniform lightingShader "material.diffuse" (TextureUnit 0)

    activeTexture $= TextureUnit 1
    textureBinding Texture2D $= Just specularMap
    setUniform lightingShader "material.specular" (TextureUnit 1)

    cubeVBO <- createCubeVBO
    containerVAO <- createContVAO cubeVBO
    lightVAO <- createLampVAO cubeVBO

    --polygonMode $= (Line, Line)
    let networkDescription :: MomentIO ()
        networkDescription = mdo
            idleE <- idleEvent w
            camB <- createAppCamera w (V3 0.0 0.0 3.0)
            reactimate $ drawScene lightingShader containerVAO lampShader lightVAO w <$> (camB <@ idleE)
    runAppLoopEx w networkDescription

    deleteObjectName lightVAO
    deleteObjectName containerVAO
    deleteObjectName cubeVBO
    terminate

drawScene :: ShaderProgram -> VertexArrayObject -> ShaderProgram -> VertexArrayObject -> AppWindow -> Camera GLfloat -> IO ()
drawScene lightingShader contVAO lampShader lightVAO w cam = do
    pollEvents
    clearColor $= Color4 0.1 0.1 0.1 1.0
    clear [ColorBuffer, DepthBuffer]

    Just time <- getTime

    -- draw the container cube
    currentProgram $= Just (program lightingShader)

    let lightPos = V3 1.2 1.0 (2.0 :: GLfloat)

    setUniform lightingShader "light.position" lightPos
    setUniform lightingShader "viewPos" (position cam)

    setUniform lightingShader "light.ambient" (V3 (0.2 :: GLfloat) 0.2 0.2)
    setUniform lightingShader "light.diffuse" (V3 (0.5 :: GLfloat) 0.5 0.5)
    setUniform lightingShader "light.specular" (V3 (1.0 :: GLfloat) 1.0 1.0)
    setUniform lightingShader "light.constant" (1.0 :: GLfloat)
    setUniform lightingShader "light.linear" (0.09 :: GLfloat)
    setUniform lightingShader "light.quadratic" (0.032 :: GLfloat)

    setUniform lightingShader "material.shininess" (32.0 :: GLfloat)

    let view = viewMatrix cam
        projection = perspective (radians (zoom cam)) (800.0 / 600.0) 0.1 (100.0 :: GLfloat)
    setUniform lightingShader "view" view
    setUniform lightingShader "projection" projection

    withVAO contVAO $ mapM_ (drawCube lightingShader) [0..9]

    -- draw the lamp
    currentProgram $= Just (program lampShader)
    setUniform lampShader "view" view
    setUniform lampShader "projection" projection

    let model = mkTransformationMat (0.2 *!! identity) lightPos
    setUniform lampShader "model" model
    withVAO lightVAO $ drawArrays Triangles 0 36

    swap w

drawCube :: ShaderProgram -> Int -> IO ()
drawCube shader i = do
    let angle = pi / 180.0 * 20.0 * fromIntegral i
        rot = axisAngle (V3 (1.0 :: GLfloat) 0.3 0.5) (realToFrac angle)
        model = mkTransformation rot (cubePositions !! i)
    setUniform shader "model" model
    drawArrays Triangles 0 36

createCubeVBO :: IO BufferObject
createCubeVBO = makeBuffer ArrayBuffer cubeWithNormalsAndTexture

createContVAO :: BufferObject -> IO VertexArrayObject
createContVAO vbo = do
    vao <- genObjectName
    bindVertexArrayObject $= Just vao
    bindBuffer ArrayBuffer $= Just vbo
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (8*4) offset0)
    vertexAttribArray (AttribLocation 0) $= Enabled
    vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 3 Float (8*4) (offsetPtr (3*4)))
    vertexAttribArray (AttribLocation 1) $= Enabled
    vertexAttribPointer (AttribLocation 2) $= (ToFloat, VertexArrayDescriptor 3 Float (8*4) (offsetPtr (6*4)))
    vertexAttribArray (AttribLocation 2) $= Enabled
    bindVertexArrayObject $= Nothing
    return vao

createLampVAO :: BufferObject -> IO VertexArrayObject
createLampVAO vbo = do
    vao <- genObjectName
    bindVertexArrayObject $= Just vao
    bindBuffer ArrayBuffer $= Just vbo
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (8*4) offset0)
    vertexAttribArray (AttribLocation 0) $= Enabled
    bindVertexArrayObject $= Nothing
    return vao
