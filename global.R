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
library(cuyem)

# Source
source('./R/sum_FCHN_redds.R') # custom query
source('./R/summariseSGS.R') # custom query
source('./R/summariseRST.R') # custom query
source('./R/summariseAGE.R') # summary page function
source('./R/queryWindowCnts.R')
source('./R/cdms_api_keys.R')

# Load Static Data
load('./data/deploy_time.rda') # update time deployed.
load('./data/datasets.rda')
load('./data/SGSRedd.rda') # cleaned with cuyem
load('./data/SGSCarcass.rda')# cleaned with cuyem
load('./data/NPTAge.rda')
load('./data/NPTRST.rda')
load('./data/NPTJuvSurvival.rda')
load('./data/NPTSturgeon.rda')
load('./data/LampreyData.rda')
load('./data/AdultWeirData.rda')
load('./data/AdultWeirData_clean.rda') # cleaned with cuyem
load('./data/NPTweir.rda')
load('./data/p_weir_df.rda') # plotly weir summary
load('./data/weir_props.rda') # weir proportions
load('./data/weir_sum_all.rda')
load('./data/SGSsummary.rda')
load('./data/JUVsummary.rda')
load('./data/AGEsummary.rda')
load('./data/FCHNsummary.rda')
load('./data/RSTcq.rda')
load('./data/users.rda')

# Login Credentials
keys <- cdmsKeys()
cdms_host <- keys[1]
# cdms_host <- 'http://localhost:80/'  # use this to access local/DEV SQL server
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

# Set login variables
login_status <- NULL
html_code <- NULL
user_info <- NULL
raw_dat <- NULL

# plotly font for graph titles
plotly_font <<- list(family = 'Balto') # family, size, color

# establish reactive values
RV <- reactiveValues(sgs_data = NULL,
                     juv_data = NULL,
                     query_data = NULL,
                     fins_data = NULL,
                     weir_sum = NULL)

  # Initial login without restricted permissions
startup_status <- cdmsLogin(username, api_key, cdms_host = cdms_host)
html_code <- status_code(startup_status)
user_info <- httr::content(startup_status, "parsed", encoding = "UTF-8")[[3]]


# Custom Query df
  query_names <- c('-Select Custom Query-', 'Fall Chinook Redd Summary', 'RST Summary', 'Redd Summary', 'SGS Summary')

  query_descriptions <- c('Choose a dataset to see description.', 'Summarized yearly aerial Fall Chinook redd counts per RKM.',
                          'Combined abundance and survival data summaries.', 'Summarized Redd data based on user-selected grouping variables.',
                          'Combined redd and carcass data summarized by population.')
  
  query_df <- c(NA_character_, 'FCHNsummary', 'RSTcq', 'redd_summary_placeholder', 'SGSsummary')

  custom_query_df <- data.frame(query_names, query_descriptions, query_df)

  