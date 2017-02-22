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
  Csv.encodeByName transactionHeader . fmap transactionBody . runTransactions

transactionBody :: Transaction -> HashMap Text Text
transactionBody Transaction {..} =
  HashMap.fromList
    [ ("日時", tshow tDay)
    , ("借方", unCategoryName . cName . unDebitCategory $ tDebit)
    , ("貸方", unCategoryName . cName . unCreditCategory $ tCredit)
    , ("摘要", unDescription tDescription)
    , ("細目", unSubDescription tSubDescription)
    , ("金額", tshow $ unAmount tAmount)
    ]

transactionHeader :: Csv.Header
transactionHeader =
  Vector.fromList
    [ encodeUtf8 "日時"
    , encodeUtf8 "借方"
    , encodeUtf8 "貸方"
    , encodeUtf8 "摘要"
    , encodeUtf8 "細目"
    , encodeUtf8 "金額"
    ]
