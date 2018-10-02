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
#library(DBI)
#library(odbc)
#library(leaflet)
#library(RColorBrewer)
library(httr)
library(jsonlite)
library(cdmsR)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {


  #-----------------------------------------------------------------
  # Login landing page
  #-----------------------------------------------------------------
  showModal(modalDialog(
    textInput('username','Username'),
    passwordInput('password', 'Password'),
    actionButton('login', 'Login'),
    size = "m",
    easyClose = FALSE,
    title = "DFRM Fisheries Data Access",
    footer = "Please contant Clark Watry (clarkw@nezperce.org) to request login credentials."
    ))

   api_key <- "153054453130053281239582410943958241094537726538860542262540750542640375910349488180619663"

   login_status <- NULL
   makeReactiveBinding("login_status")

   user_info <- NULL
   makeReactiveBinding("user_info")

   observeEvent(input$login, {

    if(input$username == '' | input$password == '')
     {
       showModal(modalDialog(
         textInput('username','Username'),
         passwordInput('password', 'Password'),
         actionButton('login', 'Login'),
         size = "m",
         easyClose = FALSE,
         title = "Username or password fields are blank.",
         footer = "Please fill in your username and password correctly."
       ))
     } else {
       login_status <<- cdmsLogin(input$username, api_key, cdms_host = 'https://cdms.nptfisheries.org')

       if(status_code(login_status) != 200) {
         showModal(modalDialog(
           textInput('username','Username'),
           passwordInput('password', 'Password'),
           actionButton('login', 'Login'),
           size = "m",
           easyClose = FALSE,
           title = "Invalid Username or Password",
           footer = "I'm sorry you are having trouble. Try re-checking the post-it note on your computer screen."
         ))
       } else {
         removeModal()
         user_info <<- content(login_status, "parsed", "application/json", encoding = "UTF-8")[[3]]
         output$username <- renderText({user_info$FullName})
       }

      }

  })

   #output$username <- renderText({"user_info$FullName"}) #user_info$FullName
   
  #-----------------------------------------------------------------
  #  Spawning Ground Survey
  #-----------------------------------------------------------------
  
   load('./data/ictrt_table.rda')
   
   output$sgs_dataset_menu <- renderUI({
     
     datasets <- getDatastores()
     
     radioButtons('sgs_datasets', h3("Dataset:"), inline = TRUE,
                  choiceNames = datasets$Name,
                  choiceValues = datasets$Id)
   })
   
   
  mpg_codes <- reactive({
    ictrt %>%
      filter(Species == input$sgs_spp,
             Run == input$sgs_run) %>%
      distinct(MPG) %>%
      arrange() %>%
      pull()
})
   
 output$mpg_menu <- renderUI({
   selectInput('sgs_mpg', h3("Major Population Groups:"), mpg_codes(), multiple=TRUE, selectize=FALSE, size = 8)
})

 pop_codes <- reactive({
   ictrt %>%
     filter(Species == input$sgs_spp & 
              Run == input$sgs_run &
              MPG %in% input$sgs_mpg) %>%
     distinct(POP_NAME) %>%
     arrange() %>%
     pull()
 })
 
 
 output$pop_menu <- renderUI({ 
   selectInput('sgs_pop', h3("Populations:"), pop_codes(), multiple=TRUE, selectize=FALSE, size = 8)
 }) 
 
 
 stream_codes <- reactive({
   ictrt %>%
     filter(Species == input$sgs_spp & 
              Run == input$sgs_run &
              MPG %in% input$sgs_mpg,
              POP_NAME %in% input$sgs_pop) %>%
     distinct(StreamTrib) %>%
     arrange() %>%
     pull()
 })
 
 output$stream_menu <- renderUI({ 
   #tmp_names <- pop_codes()
   selectInput('sgs_stream', h3("Stream : TributaryTo"), stream_codes(), multiple=TRUE, selectize=FALSE, size = 8)
 }) 
 

 observeEvent(input$sgs_submit,{
   sgs_full_df <- getDatasetView(datastoreID = input$sgs_datasets, Species = input$sgs_spp, Run = input$sgs_run,
                                 SurveyYear = input$sgs_year, MPG = input$sgs_mpg, POP = input$sgs_pop, StreamName = input$sgs_stream)
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
 
 #-------------------------------------------------------------------------
# gather sgs input values OLD STUFF
#--------------------------------------------------------------------------
# sgs_df <- eventReactive(input$sgs_submit, {
#   
#   tmp_data <- input$sgs_data
#   tmp_spp <- gsub(", ", "', '", toString(input$sgs_spp))
#   tmp_run <- gsub(", ", "', '", toString(input$sgs_run))
#   tmp_mpg <- gsub(", ", "', '", toString(str_replace(input$sgs_mpg, " :.*", "")))
#   tmp_pop <- gsub(", ", "', '", toString(str_replace(input$sgs_pop, " :.*", "")))
#   tmp_stream <- gsub(", ", "', '", toString(str_replace(input$sgs_stream, " :.*", "")))
# 
#   qry <- paste0("SELECT * FROM dbo.",tmp_data,
#               " WHERE SpeciesName = '",tmp_spp,"' AND Run = '", tmp_run,
#               "' AND SurveyYear BETWEEN ",input$sgs_year[1], " AND ", input$sgs_year[2],
#               " AND MPG IN('", tmp_mpg, "') AND POP_NAME IN ('", tmp_pop, "') AND StreamName IN('", tmp_stream,"')")
# 
#   dbGetQuery(con, qry)
# })

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
        fitBounds(long_rng[1], lat_rng[1], long_rng[2], lat_rng[2]) %>%        
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
