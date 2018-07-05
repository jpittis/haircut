{-# LANGUAGE MultiParamTypeClasses #-}
module Generic
    ( Config(..)
    , GenCircuitBreaker
    , newCircuitBreaker
    , withCircuitBreaker
    ) where

data Config = Config
  { errorThreshold :: Int
  , errorTimeout   :: Int
  }

class GenCircuitBreaker circuit where
  newCircuitBreaker  :: Config -> IO circuit
  withCircuitBreaker :: circuit -> IO a -> IO (Maybe a)

