module Main where

import LOGL.Application
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

        Just time <- getTime
        let angle = time *  pi / 180 * 50
            rot = axisAngle (V3 (0.5 :: GLfloat) 1.0 0.0) (realToFrac angle)
            model = mkTransformation rot (V3 0.0 0.0 (0.0 :: GLfloat))
            view = mkTransformationMat identity (V3 0.0 0.0 (-3.0 :: GLfloat))
            projection = perspective (pi / 4.0) (800.0 / 600.0) 0.1 (100.0 :: GLfloat)
        setUniform shader "model" model
        setUniform shader "view" view
        setUniform shader "projection" projection

        withVAO vao $ drawArrays Triangles 0 36

        swap w
    deleteObjectName vao
    deleteObjectName vbo
    terminate

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
