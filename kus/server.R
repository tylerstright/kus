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

# SGS dbase connection and cascading menus   
con <- dbConnect(odbc(),
                 Driver = "SQL Server Native Client 11.0",
                 Server = "DFRM-SQL.nezperce.org",
                 Database = "sgs_master",
                 UID = "guest",
                 PWD = "guest")

spp_code <- reactive({
  qry <- paste0("SELECT DISTINCT SpeciesCode FROM dbo.luptbl_Species WHERE SpeciesName = '", input$sgs_spp, "' AND Run = '", input$sgs_run, "'")#input$sgs_spp)

    dbGetQuery(con, qry) %>%
    pull()
})

#isolate({
  mpg_codes <- reactive({
   qry <- paste0("SELECT DISTINCT MPG FROM dbo.tbl_MgmtDesignation WHERE SpeciesRun_Code = '", spp_code(),"'")

   dbGetQuery(con, qry) %>%
             pull(MPG)
})
#  mpg_codes()
# })
   
 output$mpg_menu <- renderUI({ 
   #tmp_names <- mpg_codes()
   selectInput('sgs_mpg', h3("Major Populations:"), mpg_codes(), multiple=TRUE, selectize=FALSE)
})

 pop_codes <- reactive({
   
   mpg_qry <- gsub(", ", "', '", toString(mpg_codes()))
   
   qry <- paste0("SELECT DISTINCT POP_NAME FROM dbo.tbl_MgmtDesignation WHERE MPG IN ('", mpg_qry,"')")
   
   dbGetQuery(con, qry) %>%
     pull(POP_NAME)
 })
 
 
 output$pop_menu <- renderUI({ 
   #tmp_names <- pop_codes()
   selectInput('sgs_pop', h3("Populations:"), pop_codes(), multiple=TRUE, selectize=FALSE)
 }) 
 
 
 stream_codes <- reactive({
   
   pop_qry <- gsub(", ", "', '", toString(pop_codes()))
   
   qry <- paste0("SELECT DISTINCT StreamName, TributaryTo FROM dbo.transect_metadata WHERE POP_NAME IN ('", pop_qry,"')")
   
   dbGetQuery(con, qry) %>%
     mutate(stream = paste0(StreamName, " : ", TributaryTo)) %>%
     pull(stream)
 })
 
 
 output$stream_menu <- renderUI({ 
   #tmp_names <- pop_codes()
   selectInput('sgs_stream', h3("Stream : TributaryTo"), stream_codes(), multiple=TRUE, selectize=FALSE)
 }) 
 
output$spp_test <- renderPrint({
  #mpg_codes()
  stream_codes()
})


# })

# 
# qry <- "SELECT Mgmt_ID, Species, Run, SpeciesRun_Code, ESU_DPS, MPG, POP_NAME FROM dbo.tbl_MgmtDesignation ORDER BY POP_NAME"
# pop_name <- dbGetQuery(con, qry)
# 
# qry <- "SELECT DISTINCT Stream_ID, StreamName, TributaryTo FROM dbo.tbl_Streams ORDER BY StreamName"
# stream_name <- dbGetQuery(con, qry) %>%
#   mutate(stream_trib = paste0(StreamName, ' : ', TributaryTo)) %>%
#   select(Stream_ID, stream_trib)
# 
# qry <- "SELECT DISTINCT YEAR(SurveyDate) FROM dbo.tbl_SurveyInfo"
# survey_yrs <- na.omit(dbGetQuery(con, qry))


# gather sgs input values
sgs_df <- eventReactive(input$sgs_submit, {
  
# switch(input$sgs_data,
#          redd_detail = 'Spatial Redd Data',
#          redd_summary = 'Redd Summary',
#          carcass_summary = 'Carcass Data')
  # tmp_table <- input$sgs_data
  # tmp_spp <- input$sgs_spp
  # tmp_run <- input$sgs_run
  # tmp_year <- input$sgs_year
  # tmp_mpg <- input$sgs_mpg
  # tmp_pop <- input$sgs_pop
  # tmp_stream <- input$sgs_stream
  
  #dfset <- "redd_detail"
  #spp <- "Chinook salmon"
  qry <- paste0("SELECT * FROM dbo.",input$sgs_data," WHERE SpeciesName = '",input$sgs_spp,"' AND
                SurveyYear BETWEEN ",input$sgs_year[1], " AND ", input$sgs_year[2])
  #qry <- paste0("SELECT * FROM dbo.",dfset," WHERE SpeciesName = '",spp,"' AND SurveyYear = 2017")
  dbGetQuery(con, qry)
})

output$sgs_timeseries <- renderPlot({
  sgs_df() %>%
    group_by(Species, Run, StreamName, TributaryTo, SurveyYear) %>%
    summarise(total = sum(NewReddCount)) %>%
    ggplot(aes(x = SurveyYear, y = total, group = StreamName)) +
    geom_line() +
    geom_point() +
    facet_grid(Species~Run)
})

output$sgs_table <- renderTable({
  sgs_df()
})

# function for downloading data
output$sgs_export <- downloadHandler(
  filename = function() {
    paste0(input$sgs_data,"_", Sys.Date(), "_.csv")
  },
  content = function(file) {
    write.csv(sgs_df(), file, row.names = FALSE)
  }
)


})
