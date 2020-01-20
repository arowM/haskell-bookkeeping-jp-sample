module Main where

import ClassyPrelude

import qualified Csv
import qualified Expenses
import qualified Sales

main :: IO ()
main =
  writeFileUtf8 "dist/transactions-2016.csv" $
  Csv.fromTransactions $ do
    Expenses.transactions 2016
    Sales.transactions 2016
