{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Network.SMTP.Server.CStateT where
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Monad.State.Class
import Control.Applicative
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control
import Control.Monad.Base

newtype CStateT s m a = CStateT { unCStateT :: ReaderT (TVar s) m a }
    deriving ( Monad, Functor, Applicative, MonadIO
             , MonadTrans, MonadThrow, MonadResource
             )

instance MonadTransControl (CStateT s) where
  newtype StT (CStateT s) a = StTCState { unStTCStateT :: StT (ReaderT (TVar s)) a }
  liftWith f = CStateT $ liftWith $ \toSt -> f (liftM StTCState . toSt . unCStateT)
  restoreT = CStateT . restoreT . liftM unStTCStateT

{-
-- * liftWith law (1)
-- liftWith . const . return $ runner
--   == liftWith $ const (return runner)
--   == CStateT $ liftWith $ \toSt -> const (return runner) (liftM ...)
--   == CStateT $ liftWith $ \toSt -> return runner
--   == CStateT $ liftWith $ const $ return $ runner
--   == CStateT $ (liftWith . const . return) $ runner
--   == CStateT $ return runner
--   == return runner (QED)
--
-- * Lemma
-- liftWith (const m)
--   == CStateT $ liftWith $ \toSt -> const m (liftM..)
--   == CStateT $ liftWith (const m)
-- liftWith . const . f = CStateT $ liftWith . const . f
--
-- * liftWith law (2)
-- liftWith (const (m >>= f))
--   == CStateT $ liftWith $ \toSt -> const (m >>= f) (liftM ...)
--   == CStateT $ liftWith $ \toSt -> (m >>= f)
--   == CStateT $ liftWith $ const (m >>= f)
--   == CStateT $ liftWith (const m) >>= liftWith . const . f
--   == CStateT (liftWith (const m)) >>= CStateT (liftWith . const . f)
--   == liftWith (const m) >>= liftWith . const . f
-- 
-- * restoreT law
-- liftWith (\run -> run t) >>= restoreT . return
--   == CStateT (liftWith $ \toSt -> (\run -> run t) (liftM ...)) >>= restoreT . return
--   == CStateT (liftWith $ \toSt -> ...) >>= CStateT . restoreT . liftM unStTCStateT . return
--   == CStateT $ liftWith (\toSt -> liftM StTCState . toSt . unCStateT $ t) >>= restoreT . liftM unStTCStateT . return
--   == OK
-}

deriving instance MonadBase IO m => MonadBase IO (CStateT s m)


instance MonadBaseControl IO m => MonadBaseControl IO (CStateT s m) where
  newtype StM (CStateT s m) a = StCStateT { unStCStateT :: ComposeSt (CStateT s) m a }
  liftBaseWith = defaultLiftBaseWith StCStateT
  restoreM     = defaultRestoreM   unStCStateT


instance MonadIO m => MonadState s (CStateT s m) where
    get   = CStateT $ ask >>= liftIO . readTVarIO
    put s = CStateT $ ask >>= liftIO . atomically . flip writeTVar s

runCStateT :: MonadIO m => CStateT s m a -> s -> m (a, s)
runCStateT act s = do
  tvar  <- liftIO $ newTVarIO s
  ans   <- (runReaderT (unCStateT act) tvar)
  state <- liftIO $ readTVarIO tvar
  return (ans, state)

evalCStateT :: MonadIO m => CStateT s m a -> s -> m a
evalCStateT act s = do
  tvar  <- liftIO $ newTVarIO s
  runReaderT (unCStateT act) tvar

execCStateT :: MonadIO m => CStateT s m a -> s -> m s
execCStateT act s = do
  tvar  <- liftIO $ newTVarIO s
  _ <- runReaderT (unCStateT act) tvar
  liftIO $ readTVarIO tvar
