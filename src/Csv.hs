module Csv
  ( fromTransactions
  ) where

import Business.Bookkeeping
import ClassyPrelude
import qualified Data.Csv as Csv
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

fromTransactions :: Transactions -> ByteString
fromTransactions =
  toStrict .
  Csv.encodeByName transactionHeader . fmap transactionBody . zip [1..] . runTransactions

transactionBody :: (Int, Transaction) -> HashMap Text Text
transactionBody (n, Transaction {..}) =
  HashMap.fromList
    [ ("取引No", tshow n)
    , ("取引日", pack $ formatTime defaultTimeLocale "%Y/%m/%d" tDay)
    , ("借方勘定科目", unCategoryName . cName . unDebitCategory $ tDebit)
    , ("貸方勘定科目", unCategoryName . cName . unCreditCategory $ tCredit)
    , ("摘要", unDescription tDescription)
    , ("細目", unSubDescription tSubDescription)
    , ("貸借金額", tshow $ unAmount tAmount)
    ]

transactionHeader :: Csv.Header
transactionHeader =
  Vector.fromList
    [ encodeUtf8 "取引No"
    , encodeUtf8 "取引日"
    , encodeUtf8 "借方勘定科目"
    , encodeUtf8 "貸方勘定科目"
    , encodeUtf8 "摘要"
    , encodeUtf8 "細目"
    , encodeUtf8 "貸借金額"
    ]
