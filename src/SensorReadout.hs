{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module SensorReadout where

import Text.Trifecta
import Control.Applicative
import Text.RawString.QQ
import Data.Map (Map)
import qualified Data.Map as M

data Humidity a = Humidity a | Fail deriving (Show)
type SensorId = String

data Readout a = Readout SensorId (Humidity a) deriving (Show)

instance Functor Humidity where
    fmap fa2b (Humidity a) = Humidity $ fa2b a
    fmap _ Fail = Fail

instance Applicative Humidity where
    pure = Humidity
    (Humidity fa2b) <*> a = fmap fa2b a
    Fail <*> _ = Fail


parseHumidity :: Parser (Humidity Int)
parseHumidity = (Humidity . fromIntegral) <$> integer <|> fmap (\_ -> Fail) (char 'N' *> char 'a' *> char 'N')

parseHeader :: Parser String
parseHeader = do
    header <- some (noneOf ",\n")
    (skipSome $ char ',') <|> pure ()
    return header

parseReadout :: Parser (SensorId, Humidity Int)
parseReadout = do
    sensorId <- some (noneOf ",")
    skipSome $ char ','
    humidity <- parseHumidity
    (skipMany (oneOf "\n")) <|> pure ()
    return $ (sensorId, humidity)

mergeReadout :: (Num a) => Humidity a -> Humidity a -> Humidity a
mergeReadout a b = liftA2 (+) a b


parseAllReadouts :: Parser (Map SensorId (Humidity Int))
parseAllReadouts = do
    skipMany (oneOf "\n")
    some parseHeader
    skipMany (oneOf "\n")
    readouts <- some (parseReadout)
    return $ M.fromListWith (mergeReadout) readouts


eitherOr :: String
eitherOr = [r|
sensor-id,humidity
s1,100
s2,NaN
s3,200
s1,200|]









