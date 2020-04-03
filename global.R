# Load Packages ---- 
library(shiny)
library(shinydashboard)
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(plotly)
library(shinyjs)
library(viridis)
library(markdown)
library(DT)
library(kableExtra) # Markdown reports

# GitHub
library(cdmsR)
#library(cuyem)

# Source
source('./R/sum_FCHN_redds.R') # custom query
source('./R/summariseSGS.R') # custom query
source('./R/summariseRST.R') # custom query
source('./R/summariseAGE.R') # summary page function
source('./R/cdms_api_keys.R')

keys <- cdmsKeys()
cdms_host <- keys[1]
#cdms_host <- 'http://localhost:80/'  # use this to access local/DEV SQL server
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
raw_dat <- NULL

# plotly font for graph titles
plotly_font <<- list(family = 'Balto') # family, size, color

# establish reactive values
RV <- reactiveValues(sgs_data = NULL,
                     juv_data = NULL,
                     query_data = NULL)

  # Initial login without restricted permissions
startup_status <- cdmsLogin(username, api_key, cdms_host = cdms_host)
html_code <- status_code(startup_status)
user_info <- httr::content(startup_status, "parsed", encoding = "UTF-8")[[3]]


# Custom Query df
  query_names <- c('-Select Custom Query-', 'Fall Chinook Redd Summary', 'RST Summary', 'SGS Summary')
  
  query_descriptions <- c('Choose a dataset to see description.', 'Summarized yearly aerial Fall Chinook redd counts per RKM.', 
                          'Combined abundance and survival data summaries.', 'Combined redd and carcass data summarized by population.')
  
  custom_query_df <- tibble(query_names, query_descriptions)
  