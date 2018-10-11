{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module SensorReadout where

import Text.Trifecta
import Control.Applicative
import Text.RawString.QQ
import Data.Map (Map)
import qualified Data.Map as M

data Humidity = Humidity Int | Fail deriving (Show)
type SensorId = String

data Readout = Readout SensorId Humidity deriving (Show)



parseHumidity :: Parser Humidity
parseHumidity = (Humidity . fromIntegral) <$> integer <|> fmap (\_ -> Fail) (char 'N' *> char 'a' *> char 'N')

parseHeader :: Parser String
parseHeader = do
    header <- some (noneOf ",\n")
    (skipSome $ char ',') <|> pure ()
    return header

parseReadout :: Parser (SensorId, Readout)
parseReadout = do
    sensorId <- some (noneOf ",")
    skipSome $ char ','
    humidity <- parseHumidity
    (skipMany (oneOf "\n")) <|> pure ()
    return $ (sensorId, Readout sensorId humidity)


parseAllReadouts :: Parser (Map SensorId Readout)
parseAllReadouts = do
    skipMany (oneOf "\n")
    some parseHeader
    skipMany (oneOf "\n")
    readouts <- some (parseReadout)
    return $ M.fromListWith (<>) readouts


eitherOr :: String
eitherOr = [r|
sensor-id,humidity
s1,100
s2,NaN
s3,200|]









