{-# LANGUAGE TypeFamilies #-}
module LOGL.Application.Context
(
    AppContext, emptyContext, newContext,
    loadTexture, getTexture,
    loadShader, getShader
)
where

import LOGL.Internal.Resource
import LOGL.Internal.Texture
import LOGL.Internal.Shader
import Graphics.Rendering.OpenGL.GL as GL
import Graphics.GLUtil hiding (loadTexture, loadShader)
import Control.Monad.Reader
import Control.Monad.State as St
import Control.Monad.IO.Class


data AppContext = AppContext {  shaderMgr :: Manager ShaderProgram,
                                textureMgr :: Manager TextureObject}

emptyContext = AppContext {     shaderMgr = newManager,
                                textureMgr = newManager}

newContext :: (MonadIO m) => StateT AppContext m () -> m AppContext
newContext action = execStateT action emptyContext

loadTexture :: (MonadState m, StateType m ~ AppContext, MonadIO m) => FilePath -> m ()
loadTexture path = do
    ctx <- St.get
    newMgr <- execStateT (loadResource path) (textureMgr ctx)
    St.put ctx {textureMgr = newMgr }

getTexture :: (MonadReader m, EnvType m ~ AppContext) => String -> m TextureObject
getTexture tref = do
    ctx <- ask
    return $ getResource tref $ textureMgr ctx

getShader :: (MonadReader m, EnvType m ~ AppContext) => String -> m ShaderProgram
getShader sref = do
    ctx <- ask
    return $ getResource sref $ shaderMgr ctx

loadShader :: (MonadState m, StateType m ~ AppContext, MonadIO m) => (String, FilePath, FilePath) -> m ()
loadShader path = do
    ctx <- St.get
    newMgr <- execStateT (loadResource path) (shaderMgr ctx)
    St.put ctx {shaderMgr = newMgr }
