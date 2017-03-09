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

main :: IO ()
main = do
    GLFW.init
    w <- createAppWindow 800 600 "LearnOpenGL"

    setCursorInputMode (window w) CursorInputMode'Disabled

    depthFunc $= Just Less

    lampShader <- simpleShaderProgram ("data" </> "2_Lightning" </> "1_Colors" </> "lamp.vs")
        ("data" </> "2_Lightning" </> "1_Colors" </> "lamp.frag")

    lightingShader <- simpleShaderProgram ("data" </> "2_Lightning" </> "4_Lighting-maps" </> "specular.vs")
        ("data" </> "2_Lightning" </> "4_Lighting-maps" </> "specular.frag")

    diffuseMap <- createTexture ("data" </> "2_Lightning" </> "4_Lighting-maps" </> "container2.png")
    specularMap <- createTexture ("data" </> "2_Lightning" </> "4_Lighting-maps" </> "container2_specular.png")

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

    let lightX = 1.0 + 2.0 * sin time
        lightY = sin (time / 2.0)
        lightPos = V3 (realToFrac  lightX) (realToFrac lightY) (2.0 :: GLfloat)

    setUniform lightingShader "light.position" lightPos
    setUniform lightingShader "viewPos" (position cam)

    setUniform lightingShader "light.ambient" (V3 (0.2 :: GLfloat) 0.2 0.2)
    setUniform lightingShader "light.diffuse" (V3 (0.5 :: GLfloat) 0.5 0.5)
    setUniform lightingShader "light.specular" (V3 (1.0 :: GLfloat) 1.0 1.0)

    setUniform lightingShader "material.specular" (V3 (0.5 :: GLfloat) 0.5 0.5)
    setUniform lightingShader "material.shininess" (64.0 :: GLfloat)

    let view = viewMatrix cam
        projection = perspective (radians (zoom cam)) (800.0 / 600.0) 0.1 (100.0 :: GLfloat)
    setUniform lightingShader "view" view
    setUniform lightingShader "projection" projection

    let model = identity :: M44 GLfloat
    setUniform lightingShader "model" model
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
