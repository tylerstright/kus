#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(DBI)
library(odbc)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
con <- dbConnect(odbc(),
                 Driver = "SQL Server Native Client 11.0",
                 Server = "DFRM-SQL.nezperce.org",
                 Database = "sgs_master",
                 UID = "guest",
                 PWD = "guest")

qry <- "SELECT DISTINCT Species_ID, SpeciesName, Run, SpeciesCode FROM dbo.luptbl_Species ORDER BY SpeciesName"
spp_name <- dbGetQuery(con, qry)

qry <- "SELECT Mgmt_ID, Species, Run, SpeciesRun_Code, ESU_DPS, MPG, POP_NAME FROM dbo.tbl_MgmtDesignation ORDER BY POP_NAME"
pop_name <- dbGetQuery(con, qry)

qry <- "SELECT DISTINCT Stream_ID, StreamName, TributaryTo FROM dbo.tbl_Streams ORDER BY StreamName"
stream_name <- dbGetQuery(con, qry) %>%
  mutate(stream_trib = paste0(StreamName, ' : ', TributaryTo)) %>%
  select(Stream_ID, stream_trib)

})
