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
library(leaflet)
library(RColorBrewer)

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
   selectInput('sgs_mpg', h3("Major Population Groups:"), mpg_codes(), multiple=TRUE, selectize=FALSE, size = 8)
})

 pop_codes <- reactive({
   
   mpg_qry <- gsub(", ", "', '", toString(input$sgs_mpg))
   
   qry <- paste0("SELECT DISTINCT POP_NAME FROM dbo.tbl_MgmtDesignation WHERE MPG IN ('", mpg_qry,"')")
   
   dbGetQuery(con, qry) %>%
     pull(POP_NAME)
 })
 
 
 output$pop_menu <- renderUI({ 
   #tmp_names <- pop_codes()
   selectInput('sgs_pop', h3("Populations:"), pop_codes(), multiple=TRUE, selectize=FALSE, size = 8)
 }) 
 
 
 stream_codes <- reactive({
   
   pop_qry <- gsub(", ", "', '", toString(input$sgs_pop))
   
   qry <- paste0("SELECT DISTINCT StreamName, TributaryTo FROM dbo.transect_metadata WHERE POP_NAME IN ('", pop_qry,"')")
   
   dbGetQuery(con, qry) %>%
     mutate(stream = paste0(StreamName, " : ", TributaryTo)) %>%
     pull(stream)
 })
 
 output$stream_menu <- renderUI({ 
   #tmp_names <- pop_codes()
   selectInput('sgs_stream', h3("Stream : TributaryTo"), stream_codes(), multiple=TRUE, selectize=FALSE, size = 8)
 }) 
 
 
 # field_values <- reactive({
 #   tmp_data <- input$sgs_data
 #   qry <- paste0("SELECT TOP(0) * FROM dbo.", tmp_data)
 #   tmp_names <- names(dbGetQuery(con, qry))
 #   tmp_names %>% discard(~.x %in% c("Survey_ID", "ESU_DPS", "MPG", "POP_NAME", "StreamName", "TributaryTo"))
 # })
 # 
 # output$field_menu <- renderUI({
 #   selectInput('sgs_fields', h3("Fields:"), field_values(), multiple = TRUE, selectize = FALSE, size = 8)
 # })
 
 
# gather sgs input values
sgs_df <- eventReactive(input$sgs_submit, {
  
# switch(input$sgs_data,
#          redd_detail = 'Spatial Redd Data',
#          redd_summary = 'Redd Summary',
#          carcass_summary = 'Carcass Data')
  # tmp_table <- input$sgs_data
  tmp_data <- input$sgs_data
  #tmp_names <- gsub(", ", "', '", paste0("MPG, POP_NAME, StreamName, TributaryTo", toString(input$field_values)))
  tmp_spp <- gsub(", ", "', '", toString(input$sgs_spp))
  tmp_run <- gsub(", ", "', '", toString(input$sgs_run))
  #tmp_year <- gsub(", ", "', '", toString(input$sgs_year))
  tmp_mpg <- gsub(", ", "', '", toString(str_replace(input$sgs_mpg, " :.*", "")))
  tmp_pop <- gsub(", ", "', '", toString(str_replace(input$sgs_pop, " :.*", "")))
  tmp_stream <- gsub(", ", "', '", toString(str_replace(input$sgs_stream, " :.*", "")))

#------------------
# Section is for testing
  # stream <- c("Clearwater River")
  # tmp_data <- "carcass_detail"
  # tmp_spp <- "Chinook salmon"
  # tmp_run <- "Spring/summer"
  # tmp_mpg <- "Snake River"
  # tmp_pop <- "Snake River Lower Mainstem"
  # tmp_stream <- gsub(", ", "', '", toString(stream))
  # tmp_year <- c(1986, 2017)
  # 
  # qry <- paste0("SELECT * FROM dbo.",tmp_data,
  #               " WHERE SpeciesName = '",tmp_spp,"' AND Run = '", tmp_run,
  #               "' AND SurveyYear BETWEEN ",tmp_year[1], " AND ", tmp_year[2],
  #               " AND MPG IN('", tmp_mpg, "') AND POP_NAME IN ('", tmp_pop, "') AND StreamName IN('", tmp_stream,"')")
  # 
  # tmp <- dbGetQuery(con, qry)
  
  # output$spp_test <- renderPrint({
  #   #spp_codes()
  #   #stream_codes()
  #   #input$sgs_spp
  #   str_replace(input$sgs_stream, " :.*", "")
  #   #str_split(input$sgs_stream," : ")[[1]][1]
  # })  
#--------------------
  
  qry <- paste0("SELECT * FROM dbo.",tmp_data,
              " WHERE SpeciesName = '",tmp_spp,"' AND Run = '", tmp_run,
              "' AND SurveyYear BETWEEN ",input$sgs_year[1], " AND ", input$sgs_year[2],
              " AND MPG IN('", tmp_mpg, "') AND POP_NAME IN ('", tmp_pop, "') AND StreamName IN('", tmp_stream,"')")

  dbGetQuery(con, qry)
})

    output$redd_sum_plot <- renderPlot({
        validate(need(input$sgs_data=="redd_summary", message=FALSE))

      sgs_df() %>%
        group_by(SpeciesName, Run, StreamName, TributaryTo, SurveyYear) %>%
        summarise(total = sum(NewReddCount)) %>%
        ggplot(aes(x = as.factor(SurveyYear), y = total,colour = StreamName, group = StreamName)) +
        geom_line() +
        geom_point() +
        theme_bw() +
        labs(x = 'Survey Year',
             y = 'Total Redds',
             title = 'Total redds counted by Nez Perce Tribe during multiple pass surveys'
        )
    })
    
    colorpal <- reactive({
      colorFactor("magma", sgs_df()$SurveyYears)
    })

    output$redd_detail_plot <- renderLeaflet({
      validate(need(input$sgs_data=="redd_detail", message=FALSE))
      
      tmp_redd_detail <- sgs_df() %>%
        mutate(SurveyYear = as.factor(SurveyYear),
               Longitude = as.numeric(Longitude),
               Latitude = as.numeric(Latitude))
      
      long_rng <- range(tmp_redd_detail$Longitude)
      lat_rng <- range(tmp_redd_detail$Latitude)

      pal <- colorpal()
      
      leaflet() %>%
        #fitBounds(-117.5, 43, -113, 47.8) %>%
        #fitBounds(long_rng[1], lat_rng[1], long_rng[2], lat_rng[2]) %>%        
        addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
        addCircles(data = tmp_redd_detail, lng = ~Longitude, lat = ~Latitude, color = ~pal(SurveyYear),
                    fillColor = ~pal(SurveyYear), popup = ~paste(SurveyYear)) #%>%
        # addLegend("topright", pal = pal, values = ~tmp_redd_detail$SurveyYear,
        #         title = "Survey Year")
    })
    
    output$carcass_detail_plot <- renderPlot({
      validate(need(input$sgs_data=="carcass_detail", message=FALSE))
      
      sgs_df() %>%
        ggplot(aes(x = as.factor(SurveyYear), fill = Sex)) +
        geom_bar(position = "fill") +
        theme_bw() +
        facet_wrap(~StreamName) +
        labs(x = 'Survey Year',
             y = 'Sex',
             title = 'Proportion of carcasses found during Nez Perce Tribe spawning ground surveys by sex')
    })   


output$sgs_table <- DT::renderDataTable({
  tmp_df <- sgs_df()
  DT::datatable(tmp_df, options = list(orderClasses = TRUE))
})


# function for downloading data
output$downloadData <- downloadHandler(
  filename = function() {
    paste0(input$sgs_data,"_", Sys.Date(), "_.csv")
  },
  content = function(file) {
    write.csv(sgs_df(), file, row.names = FALSE)
  },
  contentType = "text/csv"
)


})
