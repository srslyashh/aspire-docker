library(shiny)
library(shiny.router)
library(tidyverse)
library(shinythemes)
library(shinyjs)
library(htmlwidgets) #labels on interactive maps
library(tigris)     # geojoin - need
library(raster)
library(crosstalk)
library(plyr)
library(dplyr)
library(plotly)
library(fresh) # fresh is used to create a custom bootstrap application with customized font

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)
