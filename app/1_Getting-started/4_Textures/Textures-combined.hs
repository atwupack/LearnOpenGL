module Main where

import LOGL.Window
import LOGL.Texture
import Foreign.Ptr
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL as GL
import Graphics.GLUtil
import System.FilePath

vertices :: [GLfloat]
vertices = [
--Positions           Colors         Texture Coords
    0.5,  0.5, 0.0,   1.0, 0.0, 0.0,   1.0, 1.0,   -- Top Right
    0.5, -0.5, 0.0,   0.0, 1.0, 0.0,   1.0, 0.0,   -- Bottom Right
    -0.5, -0.5, 0.0,   0.0, 0.0, 1.0,   0.0, 0.0,   -- Bottom Left
    -0.5,  0.5, 0.0,   1.0, 1.0, 0.0,   0.0, 1.0 ]   -- Top Left

indices :: [GLuint]
indices = [  -- Note that we start from 0!
        0, 1, 3,   -- First Triangle
        1, 2, 3]    -- Second Triangle

main :: IO ()
main = do
    GLFW.init
    w <- createAppWindow 800 600 "LearnOpenGL"
    shader <- simpleShaderProgram ("data" </> "1_Getting-started" </> "4_Textures" </> "Textures-combined" </> "textures.vs")
        ("data" </> "1_Getting-started" </> "4_Textures" </> "Textures-combined" </> "textures.frag")
    (vao, vbo, ebo) <- createVAO

    -- load and create textures
    t0 <- createTexture ("data" </> "1_Getting-started" </> "4_Textures" </> "Textures" </> "container.jpg")
    t1 <- createTexture ("data" </> "1_Getting-started" </> "4_Textures" </> "Textures-combined" </> "awesomeface3.png")

    --polygonMode $= (Line, Line)
    runAppLoop w $ do
        pollEvents
        clearColor $= Color4 0.2 0.3 0.3 1.0
        clear [ColorBuffer]

        -- Draw our first triangle
        currentProgram $= Just (program shader)

        activeTexture $= TextureUnit 0
        textureBinding Texture2D $= Just t0
        setUniform shader "ourTexture1" (TextureUnit 0)

        activeTexture $= TextureUnit 1
        textureBinding Texture2D $= Just t1
        setUniform shader "ourTexture2" (TextureUnit 1)

        withVAO vao $ drawElements Triangles 6 UnsignedInt nullPtr

        swapBuffers w
    deleteObjectName vao
    deleteObjectName vbo
    deleteObjectName ebo
    terminate

createVAO :: IO (VertexArrayObject, BufferObject, BufferObject)
createVAO = do
    vao <- genObjectName
    bindVertexArrayObject $= Just vao
    vbo <- makeBuffer ArrayBuffer vertices
    ebo <- makeBuffer ElementArrayBuffer indices
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (8*4) offset0)
    vertexAttribArray (AttribLocation 0) $= Enabled
    vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 3 Float (8*4) (offsetPtr (3*4)))
    vertexAttribArray (AttribLocation 1) $= Enabled
    vertexAttribPointer (AttribLocation 2) $= (ToFloat, VertexArrayDescriptor 2 Float (8*4) (offsetPtr (6*4)))
    vertexAttribArray (AttribLocation 2) $= Enabled
    bindVertexArrayObject $= Nothing
    return (vao, vbo, ebo)
