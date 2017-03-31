{-# LANGUAGE TypeFamilies #-}
module LOGL.Resource
(
    Resource(..), Manager, newManager, loadResource,
    deleteResource, updateManager, getResource, deleteAll
)
where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Maybe

data Manager a = Manager { cache :: Map.Map String a }

class Resource a where
    type LoadParam a :: *
    load :: LoadParam a -> IO (String, a)
    delete :: a -> IO ()

newManager :: (Resource r) => Manager r
newManager = Manager { cache = Map.empty }

updateManager :: (MonadIO m, Resource r) => Manager r -> StateT (Manager r) m () -> m (Manager r)
updateManager mgr action = execStateT action mgr

getResource :: (MonadReader m, EnvType m ~ Manager r, Resource r) => String -> m r
getResource name = do
    mgr <- ask
    let md = Map.lookup name (cache mgr)
    return $ fromMaybe (error "Invalid resource") md

loadResource :: (MonadState m, StateType m ~ Manager r ,  MonadIO m, Resource r) => LoadParam r -> m ()
loadResource file = do
    tm <- get
    (name, res) <- liftIO $ load file
    put tm { cache = Map.insert name res (cache tm) }

deleteResource :: (MonadState m, StateType m ~ Manager r, MonadIO m, Resource r) => String -> m ()
deleteResource name = do
    tm <- get
    let mr = Map.lookup name (cache tm)
    case mr of
        Nothing -> return ()
        Just res -> do
            liftIO $ delete res
            put tm { cache = Map.delete name (cache tm) }

deleteAll :: (MonadState m, StateType m ~ Manager r, MonadIO m, Resource r) => m ()
deleteAll = do
    tm <- get
    mapM_ (liftIO . delete) (cache tm)
    put newManager
