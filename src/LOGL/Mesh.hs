module LOGL.Mesh
(

)
where

import Linear.V2
import Linear.V3
import Graphics.Rendering.OpenGL.GL as GL hiding (Vertex, normalize, position)
import Foreign.Storable
import Foreign.Ptr
import Graphics.GLUtil.BufferObjects
import Graphics.GLUtil.ShaderProgram
import Graphics.GLUtil.VertexArrayObjects

data Vertex = Vertex { position :: V3 GLfloat, normal :: V3 GLfloat, texCoords :: V2 GLfloat }
    deriving (Eq, Show)

instance Storable Vertex where
    sizeOf _ = 2 * sizeOf (undefined :: V3 GLfloat) + sizeOf (undefined :: V2 GLfloat)
    alignment _ = alignment (undefined :: GLfloat)
    poke ptr (Vertex pos norm text) = do
        poke ptr' pos
        pokeElemOff ptr' 1 norm
        pokeElemOff ptr' 3 text
        where
            ptr' = castPtr ptr
    peek ptr = Vertex <$> peek ptr' <*> peekElemOff ptr' 1 <*> peekElemOff ptr' 3
        where
            ptr' = castPtr ptr

data Texture = Texture { tobj :: TextureObject, ttype :: TextureType, tname :: String }
    deriving (Eq, Show)

data TextureType = DiffuseMap | SpecularMap | NormalMap
    deriving (Eq, Show)

data Mesh = Mesh { vertices :: [Vertex], indices :: [GLuint], textures :: [Texture], vao :: VertexArrayObject, vbo :: BufferObject, ebo :: BufferObject}
    deriving (Eq, Show)

draw :: Mesh -> ShaderProgram -> IO ()
draw mesh shader = do
    mapM_ (setTexture shader mesh)  [0..(fromIntegral (length (textures mesh)))]
    withVAO (vao mesh) $ drawElements Triangles (fromIntegral (length (indices mesh))) UnsignedInt nullPtr

setTexture :: ShaderProgram -> Mesh -> GLuint -> IO ()
setTexture shader mesh tu = do
    let text = (textures mesh) !! fromIntegral tu
        name = "material.texture_" ++ (tname text)
    activeTexture $= TextureUnit tu
    textureBinding Texture2D $= Just (tobj text)
    setUniform shader name (TextureUnit tu)


createMesh :: [Vertex] -> [GLuint] -> [Texture] -> IO Mesh
createMesh verts inds texts = do
    newVao <- genObjectName
    newVbo <- makeBuffer ArrayBuffer verts
    newEbo <- makeBuffer ElementArrayBuffer inds

    bindVertexArrayObject $= Just newVao
    bindBuffer ArrayBuffer $= Just newVbo
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (8*4) offset0)
    vertexAttribArray (AttribLocation 0) $= Enabled
    vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 3 Float (8*4) (offsetPtr (3*4)))
    vertexAttribArray (AttribLocation 1) $= Enabled
    vertexAttribPointer (AttribLocation 2) $= (ToFloat, VertexArrayDescriptor 2 Float (8*4) (offsetPtr (6*4)))
    vertexAttribArray (AttribLocation 2) $= Enabled
    bindVertexArrayObject $= Nothing
    return Mesh {
        vertices = verts, indices = inds, textures = texts, vao = newVao, vbo = newVbo, ebo = newEbo }
