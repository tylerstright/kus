# Load Packages ---- 
library(shiny)
library(shinyBS) # we don't need this if we aren't doing the click-popup modals.
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(plotly)
library(leaflet)
library(shinyjs)
library(viridis)
library(markdown)
library(sf) 

# GitHub
library(cdmsR)
#library(cuyem)

# Source
source('./R/summariseSGS.R')
source('./R/summariseRST.R')

# Javascript for "Enter" button ----
jscode <- '
$(document).keyup(function(event) {
if ((event.keyCode == 13)) {
$("#login").click();
}
});
'

# Set variables ----
# Tribal Specific
cdms_host <- 'https://cdms.nptfisheries.org'
username <- 'api_user'
api_key <- "153054453130053281239582410943958241094537726538860542262540750542640375910349488180619663"
# Login
login_status <- NULL
html_code <- NULL
user_info <- NULL
# Initial login without restricted permissions
startup_status <- cdmsLogin(username, api_key, cdms_host = cdms_host)
html_code <- status_code(startup_status)
user_info <- httr::content(startup_status, "parsed", encoding = "UTF-8")[[3]]

# Gather static session data from CDMS----
if(html_code == 200){
  datasets <- getDatastores(cdms_host = cdms_host) %>%
    rename(DatastoreId = Id, DatastoreName = Name)
}

# LEAFLET: Load Static Map, Locations and Fish Data and any trasformations ----
  load('./data/kus_static_map_data.Rdata')
  #save.image('./data/kus_static_map_data.Rdata')

  # set map icon
  # fish <- makeAwesomeIcon(icon= 'flag', markerColor = 'green', iconColor = 'white', library = "fa")

# Startup Data Retrieval from CDMS ----
  # **** DATA SHOULD NOT BE LOADED ON STARTUP - BAD LOAD TIMES! ****

  # Parsing Transect Information (locations_df exists in kus_static_map_data.Rdata (global.R))
  transect_detail <- locations_df %>%
    separate(Description, into = c('Discard', 'Transect Type', 'Transect Length', 'Transect Description'),
             sep = '- ') %>%
    mutate(`Transect Type` = gsub('; Transect Length', '', `Transect Type`),
           `Transect Length` = as.numeric(gsub(' km; Transect Description', '', `Transect Length`))) %>%
    select(Id, Label, `Transect Type`, `Transect Length`, `Transect Description`)
  
  # Redd data
  #dsv_78 <- getDatasetView(datastoreID = 78)
  # load(file = './data/dsv_78.rda')
  
  # Redd + Transect Detail (For SGS Survey Length calcs.)
  # rtd_df <- dsv_78 %>%
  #   left_join(transect_detail, by = c('LocationId'= 'Id', 'LocationLabel' = 'Label'))
  
  # Carcass data
  # dsv_79 <- getDatasetView(datastoreID = 79)
  # load(file = './data/dsv_79.rda')
  
  # Age data
  # dsv_80 <- getDatasetView(datastoreID = 80)
  # load(file = './data/dsv_80.rda')
  
  # Juvenile Abundance data
  # dsv_85 <- getDatasetView(datastoreID = 85)
  # load(file = './data/dsv_85.rda')
  
  # Juvenile Survival data
  # dsv_86 <- getDatasetView(datastoreID = 86)
  # load(file = './data/dsv_86.rda')
  
  
  # dataset_count2 <- datasets %>%
  #   filter(!DatastoreId %in% c(81:84, 88:91)) %>%
  #   summarise(Total = n()) %>%
  #   pull()
  
  
