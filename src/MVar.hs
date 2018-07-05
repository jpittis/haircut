{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE InstanceSigs               #-}
module MVar
    ( CircuitBreaker
    ) where

import Control.Monad.Reader
import Control.Concurrent.MVar
import Data.Time.Clock.POSIX   (getPOSIXTime)
import Control.Exception       (catch, IOException, throwIO)

import Generic

data State = Closed | Open | HalfOpen

data CircuitBreaker = CircuitBreaker
  { config    :: Config
  , state     :: MVar State
  , errors    :: MVar [Int]
  }

newtype CircuitBreakerT m a = CircuitBreakerT
  { unCircuitBreakerT :: ReaderT CircuitBreaker m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader CircuitBreaker )

runCircuitBreakerT :: CircuitBreaker -> CircuitBreakerT m a -> m a
runCircuitBreakerT s m = runReaderT (unCircuitBreakerT m) s

instance GenCircuitBreaker CircuitBreaker where
  newCircuitBreaker :: Config -> IO CircuitBreaker
  newCircuitBreaker config = do
    state  <- newMVar Closed
    errors <- newMVar []
    return CircuitBreaker
      { config = config
      , state  = state
      , errors = errors
      }

  withCircuitBreaker :: CircuitBreaker -> IO a -> IO (Maybe a)
  withCircuitBreaker state f = runCircuitBreakerT state (withCircuit f)

withCircuit :: IO a -> CircuitBreakerT IO (Maybe a)
withCircuit f =
  currentState >>= \case
    Open     -> withOpen f
    HalfOpen -> withHalfOpen f
    Closed   -> withClosed f
  where
    currentState :: CircuitBreakerT IO State
    currentState = asks state >>= liftIO . readMVar

withOpen :: IO a -> CircuitBreakerT IO (Maybe a)
withOpen f = errorTimeoutExpired >>= \case
  True  -> changeState HalfOpen >> dropOldErrors >> withHalfOpen f
  False -> liftIO $ ioError (userError "circuit open!")
  where
    errorTimeoutExpired :: CircuitBreakerT IO Bool
    errorTimeoutExpired = do
      current      <- liftIO currentTimestamp
      errorTimeout <- asks (errorTimeout . config)
      asks errors >>= liftIO . readMVar >>= \case
        first:_ -> return $ first + errorTimeout < current
        _       -> return True

withHalfOpen :: IO a -> CircuitBreakerT IO (Maybe a)
withHalfOpen f = do
  result <- liftIO (runAndCatch f)
  case result of
    Left e  -> changeState Open   >> return Nothing
    Right v -> changeState Closed >> (return $ Just v)

withClosed :: IO a -> CircuitBreakerT IO (Maybe a)
withClosed f = do
  result <- liftIO (runAndCatch f)
  case result of
    Left e  -> addError >> possiblyOpen >> return Nothing
    Right v -> (return $ Just v)
  where
    addError :: CircuitBreakerT IO ()
    addError = do
      current <- liftIO currentTimestamp
      errors  <- asks errors
      liftIO $ modifyMVar_ errors (\rest -> return $ current : rest)
    possiblyOpen :: CircuitBreakerT IO ()
    possiblyOpen = pastErrorThreshold >>= \case
      False -> return ()
      True  -> changeState Open
    pastErrorThreshold :: CircuitBreakerT IO Bool
    pastErrorThreshold = do
      errorsLeft     <- dropOldErrors
      errorThreshold <- asks $ errorThreshold . config
      return $ errorsLeft >= errorThreshold

dropOldErrors :: CircuitBreakerT IO Int
dropOldErrors = do
  threshold <- asks $ errorThreshold . config
  asks errors >>= liftIO . (`modifyMVar` dropOlderThan threshold)
  where
    dropOlderThan :: Int -> [Int] -> IO ([Int], Int)
    dropOlderThan threshold errors = do
      current <- currentTimestamp
      let rest = filter (\t -> t + threshold > current) errors
      return (rest, length rest)

changeState :: State -> CircuitBreakerT IO ()
changeState change = asks state >>= (\s -> liftIO $ modifyMVar_ s (const $ return change))

currentTimestamp :: IO Int
currentTimestamp = round . (* 1000) <$> getPOSIXTime

runAndCatch :: IO a -> IO (Either IOException a)
runAndCatch f = catch (Right <$> f) (\e -> return $ Left (e :: IOException))
