{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Common where

import Data.Decimal

data OrderDir = Bid | Ask deriving (Eq, Ord, Show)
newtype Price = Price Decimal deriving (Eq, Ord, Num, Show, Real)
type Position = (OrderDir, Price)
newtype Amount = Amount Decimal deriving (Eq, Ord, Num, Show)
type Order = (Position, Amount)
data OrderBook = OrderBook
 { orderBookBids :: [(Price, Amount)]
 , orderBookAsks :: [(Price, Amount)]
 }