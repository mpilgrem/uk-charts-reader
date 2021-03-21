{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad (unless)

import Data.Time (Day)
import Data.Text (Text)
import qualified Data.Text as T (concat, intercalate, pack, unpack)
import qualified Data.Text.IO as T (putStr)
import Options.Applicative
       (Parser, (<**>), auto, execParser, fullDesc, header, help, helper, info,
       long, metavar, option, optional, progDesc, short, switch)
import Web.UKCharts.Scraper
       (UKChart(..), getChart, prettyChart)

data Options = Options
  { ukChart           :: !UKChart
  , mDate             :: !(Maybe Day)
  , noCopyrightNotice :: !Bool
  }

options :: Parser Options
options = Options
  <$> option
    auto
    (long "chart"
     <> short 'c'
     <> metavar "CHART"
     <> help
       ("Official chart, one of " <> listUKCharts <> "."))
  <*> optional
    (option
       auto
       (long "date"
        <> short 'd'
        <> metavar "DATE"
        <> help
          "Date of the chart, as YYYY-MM-DD. If not provided, the current chart."))
  <*> switch
    (long "no-copyright-notice"
     <> short 'x'
     <> help "Do not display OfficialCharts.com's copyright notice.")

main :: IO ()
main = do
  opts <- execParser options'
  mChart <- getChart (ukChart opts) (mDate opts)
  case mChart of
    Just chart -> T.putStr $ prettyChart chart
    Nothing -> putStrLn "No chart found."
  unless (noCopyrightNotice opts) (T.putStr $ "\n" <> copyrightNotice)
 where
  options' =
    info
      (options <**> helper)
      (fullDesc
       <> progDesc "Extract information for CHART at DATE"
       <> header "uk-chart-reader - a scraper for OfficialCharts.com")

copyrightNotice :: Text
copyrightNotice =
  "Copyright notice\n" <>
  "(from https://www.officialcharts.com/who-we-are/copyright-notice/)\n\n" <>
  "All material contained on OfficialCharts.com is fully protected by\n" <>
  "copyright and trademark law. Any unauthorised use (including, but not\n" <>
  "limited to, reproduction, transfer, transmission or dissemination\n" <>
  "beyond what is permitted by the Official Charts Company’s subscription\n" <>
  "and licensing agreements) is strictly prohibited.\n"

listUKCharts :: String
listUKCharts =  T.unpack $ T.intercalate ", " $
  map (T.pack . show) ([minBound .. maxBound] :: [UKChart])
