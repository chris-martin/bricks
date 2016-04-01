module ChrisMartinOrg.Chron
    ( Chron(..)
    , formatChron
    , parseChron
    ) where

import Control.Monad       (mfilter)

import           Data.List          (findIndex)
import qualified Data.Text          as T
import Text.Read (readMaybe)
import           Data.Maybe         (catMaybes, isJust)

import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Format   as TimeF


data Chron = Chron
    { chronYear  :: Int
    , chronMonth :: Int
    , chronDay   :: Maybe Int
    }

formatChron :: Chron -> T.Text
formatChron c = T.unwords $ concat [[ yearW, monthW ], maybe [] pure dayWMaybe]
  where yearW = T.pack $ show $ chronYear c
        monthW = T.pack $ fst $ TimeF.months TimeF.defaultTimeLocale !! ((chronMonth c) - 1)
        dayWMaybe = (T.pack . show) <$> chronDay c

parseChron :: String -> Either String Chron
parseChron t = do
    (yearW, monthW, dayWMaybe) <- case words t of
        [a, b]    -> Right (a, b, Nothing)
        [a, b, c] -> Right (a, b, Just c)
        _ -> Left "Invalid date: must have 2 or 3 words"
    year  <- case readMaybe yearW of
        Just x -> Right x
        _ -> Left "Invalid year"
    month <- case parseMonth monthW of
        Just x -> Right x
        _ -> Left "Invalid month"
    dayMaybe <- case dayWMaybe of
        Nothing   -> Right Nothing
        Just dayW -> case parseDay year month dayW of
            Just day -> Right $ Just day
            _ -> Left "Invalid day"
    Right $ Chron year month dayMaybe
  where
    months = snd <$> TimeF.months TimeF.defaultTimeLocale
    parseMonth mW = (+ 1) <$> findIndex (== take 3 mW) months
    validYMD y m d = isJust $ Cal.fromGregorianValid (fromIntegral y) m d
    parseDay y m dayW = mfilter (validYMD y m) (readMaybe dayW)
