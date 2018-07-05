{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE InstanceSigs #-}
module TVar
    ( CircuitBreaker
    , new
    ) where

import Control.Concurrent.STM

import Data.Either             (isRight)
import Control.Exception.Safe  (catch, IOException, throwIO)
import Data.Time.Clock.POSIX   (getPOSIXTime)

import Generic

data State = Closed | Open | Half

data CircuitBreaker = CircuitBreaker
  { config :: Config
  , state  :: TVar State
  , errors :: TVar [Int]
  }

new :: Config -> IO CircuitBreaker
new = newCircuitBreaker

instance GenCircuitBreaker CircuitBreaker where
  newCircuitBreaker :: Config -> IO CircuitBreaker
  newCircuitBreaker config =
    atomically $ do
      state  <- newTVar Closed
      errors <- newTVar []
      return CircuitBreaker
        { config = config
        , state  = state
        , errors = errors
        }

  withCircuitBreaker :: CircuitBreaker -> IO a -> IO (Maybe a)
  withCircuitBreaker circuit f = do
    timestamp <- currentTimestamp
    shouldRunRequest circuit timestamp >>= \case
      False -> return Nothing
      True  -> do
        result    <- runAndCatch f
        timestamp <- currentTimestamp
        cleanup circuit timestamp (successful result)
        case result of
          Left e  -> throwIO e
          Right v -> return $ Just v
    where
      successful = isRight

shouldRunRequest :: CircuitBreaker -> Int -> IO Bool
shouldRunRequest circuit timestamp =
  atomically $ currentState circuit >>= \case
    Closed -> return True
    Open   -> possiblyOpenHalfway
    Half   -> return True
    where
      possiblyOpenHalfway :: STM Bool
      possiblyOpenHalfway = errorTimeoutExpired circuit timestamp >>= \case
        True  -> changeState circuit Half >> return True
        False -> return False

cleanup :: CircuitBreaker -> Int -> Bool -> IO ()
cleanup circuit timestamp successful =
  atomically $ do
    state <- currentState circuit
    if successful then onSuccess state else onFailure state
  where
    onSuccess Open   = return ()
    onSuccess Closed = return ()
    onSuccess Half   = changeState circuit Closed >> resetErrors circuit
    onFailure Open   = return ()
    onFailure Closed = addError circuit timestamp >> possiblyOpen circuit timestamp
    onFailure Half   = changeState circuit Open

currentState :: CircuitBreaker -> STM State
currentState = readTVar . state

changeState :: CircuitBreaker -> State -> STM ()
changeState circuit = writeTVar (state circuit)

addError :: CircuitBreaker -> Int -> STM ()
addError circuit timestamp = modifyTVar (errors circuit) (\rest -> timestamp : rest)

possiblyOpen :: CircuitBreaker -> Int -> STM ()
possiblyOpen circuit timestamp = pastErrorThreshold circuit timestamp >>= \case
  False -> return ()
  True  -> changeState circuit Open

pastErrorThreshold :: CircuitBreaker -> Int -> STM Bool
pastErrorThreshold circuit timestamp = do
  dropErrorsOlderThanTimestamp
  numErrors <- errorsLeft
  return $ numErrors >= threshold
  where
    timeout = errorTimeout . config $ circuit
    threshold = errorThreshold . config $ circuit
    errorsLeft = length <$> readTVar (errors circuit)
    dropErrorsOlderThanTimestamp = modifyTVar (errors circuit) $
      filter (\t -> t + timeout > timestamp)

resetErrors :: CircuitBreaker -> STM ()
resetErrors circuit = writeTVar (errors circuit) []

errorTimeoutExpired :: CircuitBreaker -> Int -> STM Bool
errorTimeoutExpired circuit timestamp = do
  mostRecent <- mostRecentError
  return $ (mostRecent + timeout) < timestamp
  where
    mostRecentError = maximum <$> readTVar (errors circuit)
    timeout = errorTimeout . config $ circuit

runAndCatch :: IO a -> IO (Either IOException a)
runAndCatch f = catch (Right <$> f) (\e -> return $ Left (e :: IOException))

currentTimestamp :: IO Int
currentTimestamp = round . (* 1000) <$> getPOSIXTime
