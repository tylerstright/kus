# Load Packages ---- 
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(plotly)
library(shinyjs)
library(viridis)
library(markdown)
library(sf) 
library(DT)

# GitHub
library(cdmsR)
#library(cuyem)

# Source
source('./R/summariseSGS.R')
source('./R/summariseRST.R')
source('./R/summariseAGE.R')
source('./R/cdms_api_keys.R')

keys <- cdmsKeys()
cdms_host <- keys[1]
username <- keys[2]
api_key <- keys[3]

# Javascript for "Enter" button ----
jscode <- '
$(document).keyup(function(event) {
if ((event.keyCode == 13)) {
$("#login").click();
}
});
'

# Set variables ----
  # Login
login_status <- NULL
html_code <- NULL
user_info <- NULL

  # Initial login without restricted permissions
startup_status <- cdmsLogin(username, api_key, cdms_host = cdms_host)
html_code <- status_code(startup_status)
user_info <- httr::content(startup_status, "parsed", encoding = "UTF-8")[[3]]

# Input values for Summary Pages ----
  species_list <- c("Fall Chinook salmon", 
    "Spring/Summer Chinook Salmon",
    "Summer Steelhead")
  
  population_list<- c("Asotin Creek", "Bear Valley Creek", "Big Creek", "Big Sheep Creek", "Big, Camas, and Loon Creek", 
             "Camas Creek", "Catherine Creek", "Chamberlain Creek", "Clearwater River lower mainstem", 
             "East Fork Salmon River", "East Fork South Fork Salmon River", "Grande Ronde River lower mainstem tributaries", 
             "Grande Ronde River upper mainstem", "Hells Canyon", "Imnaha River", "Imnaha River mainstem", 
             "Joseph Creek", "Lapwai/Big Canyon", "Lawyer Creek", "Lemhi River", "Little Salmon and Rapid River", 
             "Little Salmon River", "Lochsa River", "Lolo Creek", "Lookingglass Creek", "Loon Creek", "Lostine River", 
             "Lower North Fork Clearwater", "Marsh Creek", "Meadow Creek", "Middle Fork Salmon River above Indian Creek", 
             "Middle Fork Salmon River below Indian Creek", "Middle Fork Salmon River upper mainstem", "Minam River", 
             "Moose Creek", "North Fork Clearwater River", "North Fork Salmon River", "Pahsimeroi River", "Panther Creek", 
             "Polatch River", "Salmon River lower mainstem below Redfish Lake", "Salmon River upper mainstem", 
             "Salmon River upper mainstem above Redfish Lake", "Secesh River", "Selway River", "Snake River Lower Mainstem", 
             "South Fork Clearwater River", "South Fork Salmon River", "South Fork Salmon River mainstem", "Sulphur Creek", 
             "Tucannon River", "Upper North Fork Clearwater", "Upper Selway River", "Upper South Fork Clearwater", 
             "Valley Creek", "Wallowa River", "Wenaha River", "Yankee Fork")
  
# Custom Query df
  query_names <- c('-Select Custom Query-', 'RST Summary', 'SGS Summary')
  
  query_descriptions <- c('Choose a dataset to see description.', 'Combined Abundance and Survival data summaries.', 'Combined Red and Carcass data summarized by Population.')
  
  custom_query_df <- tibble(query_names, query_descriptions)
  