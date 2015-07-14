

# load libraries

library(lubridate)
library(stationaRy)
library(weatherData)
library(dplyr)
library(ggvis)
library(tidyr)
library(readr)
library(leaflet)
library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(DT)
library(stringr)
library(ggplot2)
library(scales)
library(R.utils)


# load requisite files
#allStations <- read.csv("allStations_B.csv", stringsAsFactors = F)

## can now probably replace with call as is so fast

allStations <- get_isd_stations()
allStations$stationId <-
  paste0(allStations$usaf,"-",allStations$wban)

allStations$popup <-
  sprintf(
    "<table cellpadding='4' style='line-height:1'><tr>
    <th>%1$s</th></tr>
    
    <tr align='center'><td>Start Year: %2$s</td></tr>
    <tr align='center'><td>End Year: %3$s</td></tr>
    </table>",
    allStations$name,
    allStations$begin,
    allStations$end
  )

allStations$operational <- allStations$end-allStations$begin+1

countryChoice <- sort(unique(allStations$country_name))

## container for temperature table
temp_format = htmltools::withTags(table(thead(
  tr(
    th(colspan = 1, ''),
    th(colspan = 2, 'Maximum',align = 'center'),
    th(colspan = 2, 'Minimum'),
    th(colspan = 2, 'Average')
    
  ),
  tr(
    th('Year'),
    th('Date'),
    th('Temp'),
    th('Date'),
    th('Temp'),
    th('Year'),
    th('YTD')
  )
)))

## state temps will need revision

# airports <- read_csv("airports.csv")  ## worldwide 6208
# us <- subset(airports,country=="United States")
# stateCapitals <- read_csv("stateCapitals.csv")

capitals <- read_csv("capitals.csv")



stateTemp_format = htmltools::withTags(table(thead(tr(
  th(colspan = 1, 'Rank'),
  th(colspan = 1, 'City'),
  th(colspan = 1, 'State'),
  th(colspan = 1, 'Temperature'),
  th(colspan = 1, 'Local')
  
))))
