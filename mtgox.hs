{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import qualified Data.Text as T
import System.IO
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Concurrent
import qualified Control.Exception as E
import qualified Data.Conduit as C
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.HashMap.Strict as H
import qualified Data.Foldable as F
import Network
import Data.Digest.Pure.SHA
import Data.Maybe
import Data.Either
import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Data.Aeson
import Common

key = "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
secret = "QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ=="

signRequest key secret req = req {
  requestHeaders = requestHeaders req ++ [("Rest-Key",key), ("Rest-Sign",signature)]
  } where
 msg = case requestBody req of
   RequestBodyLBS lbs -> lbs
 secretBS = either undefined (LB.fromChunks . return) $ B64.decode secret
 signature =
    B64.encode
    . B.concat . LB.toChunks
    . bytestringDigest
    . hmacSha512 secretBS
    $  msg

invertOrder markup (pos, amnt) = (invertPos pos, amnt) where
  invertPos (Bid, price) = (Ask, (price + markup))
  invertPos (Ask, price) = (Bid, (price - markup))

ordersDiff l1 l2 = go (sort l1) (sort l2) where
  go x [] = x
  go x@((xp, xa) : xs) y@((yp, ya) : ys) = case compare xp yp of
    LT -> (xp, xa) : go xs y
    EQ -> case compare xa ya of
      LT -> go xs ((yp, ya - xa) : ys)
      EQ -> go xs ys
      GT -> go ((yp, xa - ya) : xs) ys
    GT -> error $ "encountered an unknown order: " ++ show yp

parseOrders (Array l) = map parseOrder (F.toList l) where
 parseOrder (Object h) = case (H.lookup "amount" h, H.lookup "price" h,H.lookup "type" h) of
  (Just (Object amntH), Just (Object priceH), Just (String typeS)) -> ((parseType typeS, parsePrice priceH), parseAmount amntH) where
    parseType "ask" = Ask
    parseType "bid" = Bid
    parseValInt = p . fromJust . H.lookup "value_int" where
      p (String s) = read (T.unpack s)
    parsePrice = Price . parseValInt
    parseAmount = Amount . parseValInt

getOrders = fmap parseOrders $ callMtGox "1/BTCGBP/private/orders" []

placeOrder ((typ,Price price),Amount amnt) = callMtGox "1/BTCGBP/private/order/add" params where
  params = 
   [ ("type", case typ of Bid -> "bid"; Ask -> "ask")
   , ("amount_int", show amnt)
   , ("price_int", show price)
   ]
    

runBot :: Price -> [Order] -> IO ()
runBot markup knownOrders = do
  newOrders <- getOrders
  let filledOrders = ordersDiff knownOrders newOrders
  let ordersToPlace = map (invertOrder markup) filledOrders
  when (not . null $ filledOrders) $ do
    putStrLn $ "Filled orders: " ++ show filledOrders
    putStrLn $ "Placing orders: " ++ show ordersToPlace
    mapM_ placeOrder ordersToPlace
  putStr "."
  runBot markup (newOrders ++ ordersToPlace)

mkOrders :: Price -> Price -> Price -> Amount -> Int -> [Order]
mkOrders market hmarkup step amount count = 
  [((Bid,market-step*fromIntegral i-hmarkup),amount)|i<-[0..count]]
  ++ [((Ask,market+step*fromIntegral i+hmarkup),amount)|i<-[0..count]]

myOrders = mkOrders 670000 3850 3350 5000000 20

main = do
--  mapM_ placeOrder myOrders
  hSetBuffering stdout NoBuffering
  orders <- getOrders
  runBot 7700 orders

generateNonce = do
  utcTime <- getCurrentTime
  let dt = diffUTCTime utcTime (UTCTime (fromGregorian 1900 01 01) 0)
  return $ floor $ dt * 100000

extractMtGoxRes :: Value -> Value
extractMtGoxRes (Object h) | H.lookup "result" h == Just (String "success")
  = fromJust $ H.lookup "return" h

succeedEventually action = E.catch action (\err -> const (putStr "e") (err :: E.SomeException) >> threadDelay 5000000 >> succeedEventually action)

callMtGox :: String -> [(String, String)] -> IO Value
callMtGox url params = do
  succeedEventually $ withSocketsDo $ do
     nonce <- generateNonce
     request' <- parseUrl $ "https://mtgox.com/api/" ++ url
     let request = signRequest key secret $ urlEncodedBody ([("nonce", BC8.pack (show nonce))] ++ map (join (***) (BC8.pack)) params)  $ request' { method = "POST", redirectCount = 1, responseTimeout = Just 5000000
        , requestHeaders = [("User-Agent","Rotsor")]
        }
     body <- responseBody <$> (withManager $ httpLbs request)
     let res = extractMtGoxRes $ fromJust $ decode $ body
     return $! res

{- 

import Data.Maybe
{- import Network.HTTP
import Network.HTTP.Base
import Network.HTTP.Headers 
import Network.URI-}
import Data.ByteString
import Network.HTTP.Enumerator
import Network.HTTP.Types
import qualified Data.ByteString.Lazy.Char8 as L

main = do
  req0 <- parseUrl "https://mtgox.com/api/1/BTCUSD/ticker"

  let req = req0 { method = methodPost
                 , requestHeaders = [("Content-Type", "application/json")]
                 , requestBody = RequestBodyLBS "{\"longUrl\": \"http://www.google.com/\"}"
                 }

  res <- withManager $ httpLbs req

  L.putStrLn $ responseBody res

-}
