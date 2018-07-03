{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Lib
    ( Config(..)
    , CircuitBreakerState
    , newCircuitBreaker
    , withCircuitBreaker
    ) where

import Control.Monad.Reader
import Control.Concurrent.MVar
import Data.Time.Clock.POSIX   (getPOSIXTime)
import Control.Exception       (catch, IOException, throwIO)

data Config = Config
  { errorThreshold   :: Int
  , errorTimeout     :: Int
  }

data State = Closed | Open | HalfOpen

data CircuitBreakerState = CircuitBreakerState
  { config    :: Config
  , state     :: MVar State
  , errors    :: MVar [Int]
  }

newtype CircuitBreakerT m a = CircuitBreakerT
  { unCircuitBreakerT :: ReaderT CircuitBreakerState m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader CircuitBreakerState )

runCircuitBreakerT :: CircuitBreakerState -> CircuitBreakerT m a -> m a
runCircuitBreakerT s m = runReaderT (unCircuitBreakerT m) s

newCircuitBreaker :: Config -> IO CircuitBreakerState
newCircuitBreaker config = do
  state  <- newMVar Closed
  errors <- newMVar []
  return CircuitBreakerState
    { config = config
    , state  = state
    , errors = errors
    }

withCircuitBreaker :: CircuitBreakerState -> IO a -> IO a
withCircuitBreaker state f = runCircuitBreakerT state (withCircuit f)

withCircuit :: IO a -> CircuitBreakerT IO a
withCircuit f =
  currentState >>= \case
    Open     -> withOpen f
    HalfOpen -> withHalfOpen f
    Closed   -> withClosed f
  where
    currentState :: CircuitBreakerT IO State
    currentState = asks state >>= liftIO . readMVar

withOpen :: IO a -> CircuitBreakerT IO a
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

withHalfOpen :: IO a -> CircuitBreakerT IO a
withHalfOpen f = liftIO (runAndCatch f) >>= \case
  Left e  -> changeState Open   >> liftIO (throwIO e)
  Right a -> changeState Closed >> return a

withClosed :: IO a -> CircuitBreakerT IO a
withClosed f = liftIO (runAndCatch f) >>= \case
  Right a -> return a
  Left e  -> addError >> possiblyOpen >> liftIO (throwIO e)
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
