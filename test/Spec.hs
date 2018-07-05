import Test.Hspec

import qualified TVar
import qualified MVar

import Control.Concurrent (threadDelay)
import Control.Monad      (forM_)

main :: IO ()
main = hspec $
  describe "CircuitBreaker" $ do
    it "trips and recovers after errorTimeout" $ do
      circuit <- newCircuitBreaker Config { errorThreshold = 3, errorTimeout = 2 }
      forM_ [1..3] $ \_ -> do
        withCircuitBreaker circuit success `shouldReturn` Just ()
        withCircuitBreaker circuit failure `shouldThrow` anyException
        withCircuitBreaker circuit failure `shouldThrow` anyException
        withCircuitBreaker circuit failure `shouldThrow` anyException
        withCircuitBreaker circuit success `shouldReturn` Nothing
        sleep 2
        withCircuitBreaker circuit success `shouldReturn` Just ()

    it "resets after errorTimeout without tripping" $ do
      circuit <- newCircuitBreaker Config { errorThreshold = 3, errorTimeout = 2 }
      forM_ [1..3] $ \_ -> do
        withCircuitBreaker circuit success `shouldReturn` Just ()
        withCircuitBreaker circuit failure `shouldThrow` anyException
        withCircuitBreaker circuit failure `shouldThrow` anyException
        withCircuitBreaker circuit success `shouldReturn` Just ()
        sleep 2
        withCircuitBreaker circuit failure `shouldThrow` anyException
        withCircuitBreaker circuit failure `shouldThrow` anyException
        withCircuitBreaker circuit success `shouldReturn` Just ()
        sleep 2

success :: IO ()
success = return ()

failure :: IO ()
failure = ioError (userError "Boom!")

sleep :: Int -> IO ()
sleep seconds = threadDelay (seconds * 10 ^ 6)
