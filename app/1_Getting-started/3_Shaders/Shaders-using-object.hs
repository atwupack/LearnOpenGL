module Main where

import Data.Array.MArray
import Data.Array.Storable
import LOGL.Window
import Foreign.Ptr
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL as GL
import Graphics.GLUtil
import System.FilePath

vertices :: [GLfloat]
vertices = [
    -- Positions         Colors
     0.5, -0.5, 0.0,  1.0, 0.0, 0.0,   -- Bottom Right
    -0.5, -0.5, 0.0,  0.0, 1.0, 0.0,   -- Bottom Left
     0.0,  0.5, 0.0,  0.0, 0.0, 1.0    -- Top
     ]

main :: IO ()
main = do
    GLFW.init
    w <- createAppWindow 800 600 "LearnOpenGL"
    shader <- simpleShaderProgram ("data" </> "1_Getting-started" </> "3_Shaders" </> "Shaders-using-object" </> "default.vs")
        ("data" </> "1_Getting-started" </> "3_Shaders" </> "Shaders-using-object" </> "default.frag")

    (vao, vbo) <- createVAO
    runAppLoop w $ do
        pollEvents
        clearColor $= Color4 0.2 0.3 0.3 1.0
        clear [ColorBuffer]
        -- Draw our first triangle
        currentProgram $= Just (program shader)
        bindVertexArrayObject $= Just vao
        drawArrays Triangles 0 3
        bindVertexArrayObject $= Nothing
        swapBuffers w
    deleteObjectName vao
    deleteObjectName vbo
    terminate

createVAO :: IO (VertexArrayObject, BufferObject)
createVAO = do
    vao <- genObjectName
    bindVertexArrayObject $= Just vao
    vbo <-createVBO
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (6*4) nullPtr)
    vertexAttribArray (AttribLocation 0) $= Enabled
    vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 3 Float (6*4) (plusPtr nullPtr (3*4)))
    vertexAttribArray (AttribLocation 1) $= Enabled
    bindBuffer ArrayBuffer $= Nothing
    bindVertexArrayObject $= Nothing
    return (vao, vbo)

createVBO :: IO BufferObject
createVBO = do
    let ptrsize = toEnum $ size * 4
        size = length vertices
    vbo <- genObjectName
    bindBuffer ArrayBuffer $= Just vbo
    arr <- newListArray (0, size - 1) vertices
    withStorableArray arr $ \ptr -> bufferData ArrayBuffer $= (ptrsize, ptr, StaticDraw)
    return vbo
