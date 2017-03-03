module Csv
  ( fromTransactions
  ) where

import Business.Bookkeeping
import ClassyPrelude

fromTransactions :: Transactions -> Text
fromTransactions =
  unlines . (transactionHeader:) . fmap transactionBody . zip [1..] . runTransactions

transactionBody :: (Int, Transaction) -> Text
transactionBody (n, Transaction {..}) =
  intercalate ","
    [ tshow n
    , tshow $ formatTime defaultTimeLocale "%Y/%m/%d" tDay
    , formatText . unCategoryName . cName . unDebitCategory $ tDebit
    , formatText . unCategoryName . cName . unCreditCategory $ tCredit
    , formatText . unDescription $ tDescription
    , formatText . unSubDescription $ tSubDescription
    , tshow $ unAmount tAmount
    ]

formatText :: Text -> Text
formatText = (<> "\"") . ("\"" <>)

transactionHeader :: Text
transactionHeader =
  intercalate ","
    [ formatText "取引No"
    , formatText "取引日"
    , formatText "借方勘定科目"
    , formatText "貸方勘定科目"
    , formatText "摘要"
    , formatText "細目"
    , formatText "貸借金額"
    ]
