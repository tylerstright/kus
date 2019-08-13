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
source('./R/summariseSGS.R') # custom query
source('./R/summariseRST.R') # custom query
source('./R/summariseAGE.R') # summary page function
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


# Custom Query df
  query_names <- c('-Select Custom Query-', 'RST Summary', 'SGS Summary')
  
  query_descriptions <- c('Choose a dataset to see description.', 'Combined Abundance and Survival data summaries.', 'Combined Red and Carcass data summarized by Population.')
  
  custom_query_df <- tibble(query_names, query_descriptions)
  