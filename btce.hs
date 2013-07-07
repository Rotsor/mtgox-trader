{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Conduit
import Network
import Control.Applicative
import Text.JSON
import qualified Data.Foldable as F
import Common
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T


depthUrl = "https://btc-e.com/api/2/btc_usd/depth"

simpleGetUrl url = do
  withSocketsDo $ do
     request' <- parseUrl $ url
     let request = request' { method = "GET", responseTimeout = Just 5000000
        , requestHeaders = [("User-Agent","Rotsor")]
        }
     Response _status _version _headers body <- withManager $ httpLbs request
     return (body :: LBS.ByteString)

getBtceOrderBook :: IO OrderBook
getBtceOrderBook = do
  bytes <- simpleGetUrl depthUrl
  case decode bytes of
    Nothing -> error $ "Unable to decode BTC-E order book: " ++ show bytes
    Just (JSObject h) -> case OrderBook <$> getWall "bids" h <*> getWall "asks" h of
      Left err -> error err
      Right res -> return res
  where
    getWall :: T.Text -> H.HashMap T.Text JSValue -> Either String [(Price, Amount)]
    getWall name h = case H.lookup name h of
      Nothing -> Left $ "Can't find field " ++ show name
      Just (Array arr) -> mapM getPriceAmnt (F.toList arr) where
        getPriceAmnt (Array arr) = case F.toList arr of
            [Number price, Number amount] -> Right (Price price, Amount amount)
            [price,Number _] -> Left $ "Invalid price in JSON: " ++ show price
            [Number _,amount] -> Left $ "Invalid amount in JSON: " ++ show amount
            _ -> Left $ "Invalid price&amount in JSON: " ++ show arr
