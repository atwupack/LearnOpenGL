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

data Vertex = Vertex { position :: V3 GLfloat, normal :: V3 GLfloat, texCoords :: V2 GLfloat }

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

data Texture = Texture { tobj :: TextureObject, ttype :: TextureType }

data TextureType = DiffuseMap | SpecularMap | NormalMap

data Mesh = Mesh { vao :: VertexArrayObject, vbo :: BufferObject, ebo :: BufferObject}

createMesh :: [Vertex] -> [GLuint] -> [Texture] -> IO Mesh
createMesh verts indices texts = do
    vao <- genObjectName
    vbo <- makeBuffer ArrayBuffer verts
    ebo <- makeBuffer ElementArrayBuffer indices

    bindVertexArrayObject $= Just vao
    bindBuffer ArrayBuffer $= Just vbo
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (8*4) offset0)
    vertexAttribArray (AttribLocation 0) $= Enabled
    vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 3 Float (8*4) (offsetPtr (3*4)))
    vertexAttribArray (AttribLocation 1) $= Enabled
    vertexAttribPointer (AttribLocation 2) $= (ToFloat, VertexArrayDescriptor 2 Float (8*4) (offsetPtr (6*4)))
    vertexAttribArray (AttribLocation 2) $= Enabled
    bindVertexArrayObject $= Nothing
    return $ Mesh vao vbo ebo
