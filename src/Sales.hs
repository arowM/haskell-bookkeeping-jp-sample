module Sales
  ( transactions
  ) where

import Business.Bookkeeping
import Business.Bookkeeping.JP.SelfEmployed
import ClassyPrelude

transactions :: Year -> Transactions
transactions 2016 = year 2016 $ do
  _売上 (2, 20) (2, 28) "ジャパリパーク ラッキービースト改修費" 200000
  _ジャパリパーク技術顧問料 3 31
  _ジャパリパーク技術顧問料 4 30
  _ジャパリパーク技術顧問料 5 31
transactions _ = return ()

_ジャパリパーク技術顧問料 :: Month -> Date -> YearTransactions
_ジャパリパーク技術顧問料 m d =
  month m $ do
    _即日売上 d "ジャパリパーク技術顧問料" 600000
    _源泉所得税 d "ジャパリパーク技術顧問料" 50000
