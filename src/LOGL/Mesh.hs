{-# LANGUAGE TypeFamilies #-}
module LOGL.Mesh
(
    Mesh, createMesh, drawMesh, Vertex(..), deleteMesh, Texture(..), TextureType(..)
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
import Control.Monad.Reader
import Control.Monad.IO.Class
import LOGL.Window
import LOGL.Resource

data Vertex = Vertex { position :: V3 GLfloat, normal :: V3 GLfloat, texCoords :: V2 GLfloat }
    deriving (Eq, Show)

instance Storable Vertex where
    sizeOf _ = 2 * sizeOf (undefined :: V3 GLfloat) + sizeOf (undefined :: V2 GLfloat)
    alignment _ = alignment (undefined :: GLfloat)
    poke ptr (Vertex pos norm text) = do
        poke (castPtr ptr) pos
        pokeElemOff (castPtr ptr) 1 norm
        pokeElemOff (castPtr ptr) 3 text
    peek ptr = Vertex <$> peek (castPtr ptr) <*> peekElemOff (castPtr ptr) 1 <*> peekElemOff (castPtr ptr) 3

data Texture = Texture { tref :: String, ttype :: TextureType, tname :: String }
    deriving (Eq, Show)

data TextureType = DiffuseMap | SpecularMap | NormalMap
    deriving (Eq, Show)

data Mesh = Mesh { vertices :: [Vertex], indices :: [GLuint], textures :: [Texture], vao :: VertexArrayObject, vbo :: BufferObject, ebo :: BufferObject}
    deriving (Eq, Show)

deleteMesh :: Mesh -> IO ()
deleteMesh mesh = do
    deleteObjectName $ vao mesh
    deleteObjectName $ ebo mesh
    deleteObjectName $ vbo mesh

drawMesh :: (MonadReader m, EnvType m ~ AppContext, MonadIO m) => Mesh -> String -> m ()
drawMesh mesh sref = do
    setTextures sref (textures mesh)
    liftIO $ withVAO (vao mesh) $ drawElements Triangles idxCount UnsignedInt nullPtr
        where
            idxCount = fromIntegral (length (indices mesh))

setTextures ::(MonadReader m, EnvType m ~ AppContext, MonadIO m) => String -> [Texture] -> m ()
setTextures sref [] = return ()
setTextures sref texts = do
    ctx <- ask
    let shader = getResource sref $ shaderMgr ctx
    liftIO $ mapM_ (setTexture shader texts)  [0..texCount - 1]
    where
        texCount = fromIntegral (length texts)


setTexture :: ShaderProgram -> [Texture] -> GLuint -> IO ()
setTexture shader texts tu  = do
    let text = texts !! fromIntegral tu
        name = "mat." ++ tname text
    activeTexture $= TextureUnit tu
    -- textureBinding Texture2D $= Just (tobj text)
    setUniform shader name (TextureUnit tu)


createMesh :: [Vertex] -> [GLuint] -> [Texture] -> IO Mesh
createMesh verts inds texts = do
    newVao <- genObjectName
    newVbo <- makeBuffer ArrayBuffer verts
    newEbo <- makeBuffer ElementArrayBuffer inds

    bindVertexArrayObject $= Just newVao
    bindBuffer ArrayBuffer $= Just newVbo
    bindBuffer ElementArrayBuffer $= Just newEbo
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (8*4) offset0)
    vertexAttribArray (AttribLocation 0) $= Enabled
    vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 3 Float (8*4) (offsetPtr (3*4)))
    vertexAttribArray (AttribLocation 1) $= Enabled
    vertexAttribPointer (AttribLocation 2) $= (ToFloat, VertexArrayDescriptor 2 Float (8*4) (offsetPtr (6*4)))
    vertexAttribArray (AttribLocation 2) $= Enabled
    bindVertexArrayObject $= Nothing
    return Mesh {
        vertices = verts, indices = inds, textures = texts, vao = newVao, vbo = newVbo, ebo = newEbo }
