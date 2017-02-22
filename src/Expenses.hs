module Expenses
  ( transactions
  ) where

import Business.Bookkeeping
import Business.Bookkeeping.JP.SelfEmployed
import ClassyPrelude

transactions :: Year -> Transactions
transactions 2016 = year 2016 $ do
  _年間按分家賃 2016
  _年間按分電気代 2016
  month 1 $ do
    activity 2 "ジャパリパーク取材" $ do
      _旅費交通費 340
      _消耗品費 "入場料" 1000
      _消耗品費 "ジャパリバス乗車料" 400
      _消耗品費 "ジャパリまん" 200
    activity 20 "ジャパリパーク商談" $
      _旅費交通費 340

transactions _ = pure ()

{-| 年間の按分家賃
-}
_年間按分家賃 :: Year -> YearTransactions
_年間按分家賃 2016 =
  for' [1..12] $ \m ->
    month m $ _按分家賃 25
_年間按分家賃 _ = pure ()

{-| 年間の按分電気代
-}
_年間按分電気代 :: Year -> YearTransactions
_年間按分電気代 2016 = do
  month 1 $ _按分電気代 27 4000
  month 2 $ _按分電気代 26 5000
  month 3 $ _按分電気代 28 6000
  month 4 $ _按分電気代 27 4000
_年間按分電気代 _ = pure ()

-- ==================
--  Helper functions
-- ==================

reduceM
  :: (MonoFoldable mono, Monad m)
  => (Element mono -> m ()) -> mono -> m ()
reduceM f = foldl' (\m a -> m >> f a) (pure ())

for'
  :: (MonoFoldable mono, Monad m)
  => mono -> (Element mono -> m ()) -> m ()
for' = flip reduceM

_按分家賃 :: Date -> MonthTransactions
_按分家賃 d =
  activity d "按分家賃" $
    _経費 "地代家賃" "50%" (Amount $ 50000 `div` 2)

_按分電気代 :: Date -> Amount -> MonthTransactions
_按分電気代 d (Amount a) =
  activity d "按分電気代" $
    _経費 "水道光熱費" "50%" (Amount $ a `div` 2)
