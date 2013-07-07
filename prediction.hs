{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Decimal
import Data.List.Split
import Control.DeepSeq
import Data.Time
import Data.Conduit
import Data.Conduit.List
import Data.Conduit.Binary as CB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BSB
import Data.ByteString.Lex.Double
import Data.Maybe

type Time = Int
type Price = Double
type Volume = Double

data Predictor = Predictor {
  runPredictor :: Time -> (Price, Price -> Volume -> Predictor)
  }

readInt' = fst . fromJust . BS.readInt
readDouble' = fst . fromJust . readDouble
myReadDouble = finalize . BSB.foldl' fld (0 :: Int, 0 :: Int) where
 fld (l, m) 46 {- '.' -} = (l, 1)
 fld (a, m) c = (a * 10 + (fromIntegral c - 48), m * 10)

 finalize (l, 0) = fromIntegral l
 finalize (l, m) = fromIntegral l / fromIntegral m

getData = do
  runResourceT $ mapOutput (parseLine . BS.split ',') (sourceFile "mtgox_history.txt" $= CB.lines) $$ consume where
    parseLine [t, p, v] = force (readInt' t, myReadDouble p, myReadDouble v)
    parseLine err = error $ show err

{- getData = do
  f <- readFile "mtgox_history.txt"
  return $ map (parseLine . wordsBy (==',')) (lines f) where
  parseLine [t, p, v] = (read t, read p, read v)-}

lastPricePredictor :: Predictor
lastPricePredictor = go 0 where
  go p = Predictor (\t -> (p, \p' _ -> go p'))

zeroPredictor :: Predictor
zeroPredictor = Predictor (\t -> (0, \_ _ -> zeroPredictor))

expoDecay decayTime timePassed = exp (negate $ timePassed / decayTime)


expoDecayAvg :: Double -> Double -> Double -> Double -> Double
expoDecayAvg decayTime timePassed oldAvg newData = p * oldAvg + (1 - p) * newData
 where
  p = expoDecay decayTime timePassed

lastBTC :: Double -> Predictor
lastBTC window = go 0 where
  go p0 = Predictor (\_ -> (p0, \p1 v1 -> go (expoDecayAvg window v1 p0 p1)))

predictors = 
  [("Zero predictor", zeroPredictor),
   ("Last price", lastPricePredictor),
   ("Last 0.01 BTC", lastBTC 0.01),
   ("Last 0.1 BTC", lastBTC 0.1),
   ("Last 1 BTC", lastBTC 1),
   ("Last 10 BTC", lastBTC 10),
   ("Last 30 BTC", lastBTC 30),
   ("Last 100 BTC", lastBTC 100),
   ("Last 300 BTC", lastBTC 300),
   ("Last 1000 BTC", lastBTC 1000),
   ("Last 10000 BTC", lastBTC 10000)
  ]

runPredictorWithDelay delay p datas = gogogo predictions datas 0 where
  gogogo ps [] !acc = acc
  gogogo ps ((t, p, v) : rest) !acc = case break ((> t - delay) . fst) ps of
    (past, future) -> case last past of
      lp@(pt, pp) -> gogogo (lp : future) rest (acc + (p - pp) ^ 2 * v)
  predictions :: [(Time, Price)]
  predictions = (0, 0) : gogo p datas where
    gogo _ [] = []
    gogo pre ((t, p, v) : rest) = case runPredictor pre t of
       (prediction, nextPre) -> (t, prediction) : gogo (nextPre p v) rest

measureBadness :: Predictor -> IO ()
measureBadness p = do
  getCurrentTime >>= print
  d <- getData
  getCurrentTime >>= print
  print $ length d
  getCurrentTime >>= print
  deepseq d (putStrLn "data forced.")
  getCurrentTime >>= print
  deepseq d (putStrLn "data forced 2.")
  getCurrentTime >>= print
  putStrLn $ "Total volume: " ++ show (sum $ fmap (\(_,_,v) -> v) d)
  flip Prelude.mapM_ predictors $ \(pName, predictor) -> do
    putStr (pName ++ replicate (maximum (fmap (length . fst) predictors) - length pName + 1) ' ' ++ ": ")
    print (runPredictorWithDelay (3600 * 24 * 3) predictor d)
     where
      go _ [] !acc = acc
      go pre ((t, p, v) : rest) !acc = case runPredictor pre t of
        (predictedP, nextPre) -> go (nextPre p v) rest (acc + v * (predictedP - p) ^ 2)

main = measureBadness zeroPredictor >>= print
