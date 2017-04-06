module Main where

import LOGL.Application
import LOGL.Objects
import Foreign.Ptr
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL as GL
import Graphics.GLUtil
import System.FilePath
import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects
import Linear.Matrix
import Linear.V3
import Linear.Quaternion
import Linear.Projection


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

    depthFunc $= Just Less

    shader <- simpleShaderProgram ("data" </> "1_Getting-started" </> "6_Coordinate-systems" </> "coord-systems.vs")
        ("data" </> "1_Getting-started" </> "6_Coordinate-systems" </> "coord-systems.frag")
    (vao, vbo) <- createVAO

    -- load and create texture
    t0 <- createTexture ("data" </> "1_Getting-started" </> "4_Textures" </> "Textures" </> "container.jpg")
    t1 <- createTexture ("data" </> "1_Getting-started" </> "4_Textures" </> "Textures-combined" </> "awesomeface3.png")

    --polygonMode $= (Line, Line)
    runAppLoop w $ do
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

        (width, height) <- getFramebufferSize $ window w

        Just time <- getTime
        let radius = 10.0 :: GLfloat
            camX = radius * sin (realToFrac time)
            camZ = radius * cos (realToFrac time)
            view = lookAt (V3 camX 0.0 camZ) (V3 0.0 0.0 0.0) (V3 0.0 1.0 (0.0 :: GLfloat))
            projection = perspective (pi / 4.0) (fromIntegral width / fromIntegral height) 0.1 (100.0 :: GLfloat)
        setUniform shader "view" view
        setUniform shader "projection" projection

        withVAO vao $ mapM_ (drawCube shader) [0..9]

        swap w
    deleteObjectName vao
    deleteObjectName vbo
    terminate

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
