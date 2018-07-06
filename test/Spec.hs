import Test.Hspec

import qualified TVar
import qualified MVar
import Generic

import Control.Concurrent       (threadDelay)
import Control.Monad            (forM_)
import Control.Concurrent.Async (replicateConcurrently_)
import Data.Time.Clock.POSIX    (getPOSIXTime)

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

  describe "Benchmark" $ do
    it "runs a bunch of successes" $ do
      forM_ [1..10] $ \_ -> do
        print "---"
        mvar <- MVar.new Config { errorThreshold = 3, errorTimeout = 2 }
        (time $ benchmark 10000 mvar) >>= print
        tvar <- TVar.new Config { errorThreshold = 3, errorTimeout = 2 }
        (time $ benchmark 10000 tvar) >>= print

time f = do
  before <- currentTime
  f
  after <- currentTime
  return (after - before)
  where
    currentTime :: IO Int
    currentTime = round . (* 1000) <$> getPOSIXTime

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

benchmark n circuit =
  replicateConcurrently_ n (makeNRequests n)
  where
    makeNRequests n = forM_ [1..n] $ \_ ->
      withCircuitBreaker circuit success

success :: IO ()
success = return ()

failure :: IO ()
failure = ioError (userError "Boom!")

sleep :: Int -> IO ()
sleep seconds = threadDelay (seconds * 10 ^ 6)
