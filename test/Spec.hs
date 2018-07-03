import Test.Hspec

import Lib

import Control.Concurrent (threadDelay)
import Control.Monad      (forM_)

main :: IO ()
main = hspec $
  describe "CircuitBreaker" $
    it "behaves like a circuit breaker" $ do
      circuit <- newCircuitBreaker Config { errorThreshold = 3, errorTimeout = 2 }
      forM_ [1..3] $ \_ -> do
        withCircuitBreaker circuit success `shouldReturn` ()
        withCircuitBreaker circuit failure `shouldThrow` anyException
        withCircuitBreaker circuit failure `shouldThrow` anyException
        withCircuitBreaker circuit failure `shouldThrow` anyException
        withCircuitBreaker circuit success `shouldThrow` anyException
        threadDelay (2 * 10 ^ 6)
        withCircuitBreaker circuit success `shouldReturn` ()

success :: IO ()
success = return ()

failure :: IO ()
failure = ioError (userError "Boom!")
