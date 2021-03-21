{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Web.UKCharts.Scraper
Description : Scraper of www.OfficialCharts.com.
Copyright   : (c) Mike Pilgrem, 2021
License     : BSD3
Maintainer  : public@pilgrem.com
Stability   : experimental
Portability : POSIX, Windows

A library to extract, from web pages, certain information published by The
Official Charts Company Limited at <https://www.OfficialCharts.com>.

NB: OfficialCharts.com states that all material contained on that web site is
fully protected by copyright and trademark law and any unauthorised use
(including, but not limited to, reproduction, transfer, transmission or
dissemination beyond what is permitted by The Official Charts Company Limited’s
subscription and licensing agreements) is strictly prohibited. See
<https://www.officialcharts.com/who-we-are/copyright-notice/>.

This library has no connection with The Official UK Charts Company Limited or
its affiliates.
-}
module Web.UKCharts.Scraper
  ( Chart
  , Entry (..)
  , Item (..)
  , Format (..)
  , UKChart (..)
  , getChart
  , prettyChart
  , ukChartFormat
  ) where

import Data.List (foldl')
import Data.Maybe (fromMaybe)

import Data.Text (Text)
import qualified Data.Text as T (concat, length, pack, replicate, unpack)
import qualified Data.Text.Read as T (decimal)
import Data.Time (Day, defaultTimeLocale, formatTime)
import Text.HTML.Scalpel
       (scrapeURL, chroots, text, (//), (@:), hasClass, tagSelector, Scraper)

-- | Type representing entries in a chart.
data Entry = Entry
  { position :: !Int
  , item     :: !Item
  }
  deriving (Eq, Show)

-- | Type representing items in chart entries.
data Item = Item
  { format :: !Format
  , title  :: !Text
  , artist :: !(Maybe Text)
  , label  :: !Text
  }
  deriving (Eq, Show)

-- | Type representing formats of items in chart entries.
data Format
  = Single
  | Album
  | AudioVisual
  deriving (Eq, Show)

-- | Synonym for type representing charts.
type Chart = [Entry]

-- | Type representing charts published by The Official Chart Company Limited at
-- <https://www.OfficialCharts.com OfficialCharts.com>.
data UKChart
  = SinglesChartTop100
  | AlbumsChartTop100
  | CompilationsChartTop100
  deriving (Bounded, Enum, Eq, Read, Show)

ukChartUrl :: UKChart -> Text
ukChartUrl SinglesChartTop100 = "singles-chart"
ukChartUrl AlbumsChartTop100 = "albums-chart"
ukChartUrl CompilationsChartTop100 = "official-compilations-chart"

ukChartCode :: UKChart -> Text
ukChartCode SinglesChartTop100 = "7501"
ukChartCode AlbumsChartTop100 = "7502"
ukChartCode CompilationsChartTop100 = "7503"

-- | @ukChartFormat chartType@ returns the 'Format' of the given chart type.
ukChartFormat :: UKChart -> Format
ukChartFormat SinglesChartTop100 = Single
ukChartFormat AlbumsChartTop100 = Album
ukChartFormat CompilationsChartTop100 = Album

-- | @getChart chartType mDate@ returns an action that attempts to extract the
-- chart for the given chart type and date (if supplied). Attempts to extract
-- the current chart if a date is not supplied.
getChart :: UKChart -> Maybe Day -> IO (Maybe Chart)
getChart chart mDate = scrapeURL url entries
 where
  format = ukChartFormat chart

  entries :: Scraper String [Entry]
  entries =
    let selector = "table" @: [hasClass "chart-positions"] // "tr"
    in chroots selector entry

  entry :: Scraper String Entry
  entry = do
    position <- read <$> text ("span" @: [hasClass "position"])
    title <- T.pack <$> text ("div" @: [hasClass "title"] // "a")
    let hasArtist =
          Just . T.pack <$> text ("div" @: [hasClass "artist"] // "a")
    mArtist <- case format of
      Single -> hasArtist
      Album -> hasArtist
      AudioVisual -> pure Nothing
    label <- T.pack <$> text ("span" @: [hasClass "label"])
    pure $ Entry position (Item format title mArtist label)

  url :: String
  url = T.unpack $ case mDate of
    Just date
      -> let date' = T.pack $ formatTime defaultTimeLocale "%0Y%m%d" date
         in base <> date' <> "/" <> ukChartCode chart <> "/"
    Nothing -> base
   where
    base = "https://www.officialcharts.com/charts/" <> ukChartUrl chart <> "/"

-- | Returns a nicely-formatted version of the chart.
prettyChart :: Chart -> Text
prettyChart chart = headings <> T.concat (map prettyEntry chart)
 where

  (pcw, tcw, acw', lcw) = columnWidths headingWidths chart

  acw = if acw' == 0 then 0 else max (T.length "Artist") acw'

  (ph, th, ah, lh) = ( "Pos"
                     , "Title"
                     , if acw' ==0 then "" else "Artist"
                     , "Label"
                     )

  headingWidths :: (Int, Int, Int, Int)
  headingWidths = ( T.length ph
                  , T.length th
                  , 0
                  , T.length lh
                  )

  padLeft :: Int -> Text -> Text
  padLeft n t = let l = T.length t
                    n' = max 0 (n - l)
                in T.replicate n' " " <> t

  underline :: Int -> Text
  underline n = T.replicate n "-"

  headings :: Text
  headings = padLeft pcw ph <> " " <> padLeft tcw th <> " " <>
             (if acw == 0 then "" else padLeft acw ah <> " ") <>
             padLeft lcw lh <> "\n" <>
             underline pcw <> " " <> underline tcw <> " " <>
             (if acw == 0 then "" else underline acw <> " ") <>
             underline lcw <> "\n"

  prettyEntry :: Entry -> Text
  prettyEntry (Entry position (Item _ title mArtist label)) =
    padLeft pcw (T.pack $ show position) <> " " <> padLeft tcw title <> " " <>
    (if acw == 0 then "" else padLeft acw (fromMaybe "" mArtist) <> " ") <>
    padLeft lcw label <> "\n"

-- | Helper function that returns the maximum of base widths or the column
-- widths required to display the given entry.
maxWidths :: (Int, Int, Int, Int) -> Entry -> (Int, Int, Int, Int)
maxWidths (pn, tn, an, ln) entry =
  let (pn', tn', an', ln') = width entry
  in  (max pn pn', max tn tn', max an an', max ln ln')

-- | Helper function that returns the column widths required to display the
-- given entry.
width :: Entry -> (Int, Int, Int, Int)
width (Entry position (Item _ title mArtist label)) =
  ( length $ show position
  , T.length title
  , maybe 0 T.length mArtist
  , T.length label
  )

-- | Helper function that returns the column widths required to display the
-- given chart.
columnWidths :: (Int, Int, Int, Int) -> Chart -> (Int, Int, Int, Int)
columnWidths = foldl' maxWidths
