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
library(DT)

# GitHub packages
library(cdmsR)
library(cuyem)

# Modules
source('./R/divInfoUI.R')
source('./R/divInfoServer.R')
source('./R/employeeInfoUI.R')
source('./R/employeeInfoServer.R')
source('./R/contactInfoUI.R')
source('./R/contactInfoServer.R')
source('./R/windowCountUI.R')
source('./R/windowCountServer.R')

# Source
source('./R/sum_FCHN_redds.R') # custom query
source('./R/summariseRST.R') # custom query
# source('./R/summariseAGE.R') # summary page function - off until we fix dataset
source('./R/queryWindowCnts.R')
source('./R/cdms_api_keys.R')

# Load Static Data
load('./data/kus_data.rda')
load('./data/nosa_df.rda')

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
                     fins_data = NULL)

  # Initial login without restricted permissions
startup_status <- cdmsLogin(username, api_key, cdms_host = cdms_host)
html_code <- status_code(startup_status)
user_info <- httr::content(startup_status, "parsed", encoding = "UTF-8")[[3]]
cookie <- startup_status[["cookies"]][["value"]]
# cookieType <- 