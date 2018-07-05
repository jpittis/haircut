import Test.Hspec

import qualified TVar
import qualified MVar
import Generic

import Control.Concurrent (threadDelay)
import Control.Monad      (forM_)

main :: IO ()
main = hspec $ do
  describe "TVar" $ do
    it "trips and recovers after errorTimeout" $ do
      tvar <- TVar.new Config { errorThreshold = 3, errorTimeout = 2 }
      runTest1 tvar

    it "resets after errorTimeout without tripping" $ do
      tvar <- TVar.new Config { errorThreshold = 3, errorTimeout = 2 }
      runTest2 tvar

  describe "MVar" $ do
    it "trips and recovers after errorTimeout" $ do
      mvar <- MVar.new Config { errorThreshold = 3, errorTimeout = 2 }
      runTest1 mvar

    it "resets after errorTimeout without tripping" $ do
      mvar <- MVar.new Config { errorThreshold = 3, errorTimeout = 2 }
      runTest2 mvar

runTest1 circuit =
  forM_ [1..3] $ \_ -> do
    withCircuitBreaker circuit success `shouldReturn` Just ()
    withCircuitBreaker circuit failure `shouldThrow` anyException
    withCircuitBreaker circuit failure `shouldThrow` anyException
    withCircuitBreaker circuit failure `shouldThrow` anyException
    withCircuitBreaker circuit success `shouldReturn` Nothing
    sleep 2
    withCircuitBreaker circuit success `shouldReturn` Just ()

runTest2 circuit =
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
