{-# LANGUAGE TypeFamilies #-}
module LOGL.Model
(
    Model, drawModel, loadModel
)
where

import LOGL.Application.Context
import LOGL.Mesh
import Graphics.GLUtil.ShaderProgram
import Codec.Wavefront
import Control.Monad.Reader
import Control.Monad.IO.Class

data Model = Model { meshes :: [Mesh], directory :: FilePath} deriving (Eq, Show)

drawModel :: (MonadReader m, EnvType m ~ AppContext, MonadIO m) => Model -> String -> m ()
drawModel model shader = mapM_ (\x -> drawMesh x shader) (meshes model)

loadModel :: FilePath -> IO ()
loadModel path = do
    Right wfo <- fromFile path
    print $ length (objLocations wfo)
    print $ length (objPoints wfo)
    print $ length (objLines wfo)
    --print $ objFaces wfo
