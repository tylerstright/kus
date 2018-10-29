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
library(httr)
#devtools::install_github('ryankinzer/cdmsR')
#library(cdmsR)

source('./R/cdmsLogin.R')
source('./R/getDatastores.R')
source('./R/getProjects.R')
source('./R/getLocations.R')
source('./R/getWaterbodies.R')
source('./R/getDatasetView.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Need to set tribal specific variables
  cdms_host <- 'https://cdms.nptfisheries.org'
  
  # Development only.
  #cdms_host <- 'localhost'
  #api_key <- "153054453130053281239582410943958241094537726538860542262540750542640375910349488180619663"
  
  login_status <- NULL
  makeReactiveBinding("login_status")
  
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
    footer = "Please contact Clark Watry (clarkw@nezperce.org) to request login credentials."
    ))

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
       login_status <<- cdmsLogin(input$username, input$password, cdms_host = cdms_host)  #input$password

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
         #login_status
         # output$home_buttons <- renderUI({
         #   div(class = 'homebutton', style="display: inline-block;", "Fish Management")
         #   div(class = 'homebutton', style="display: inline-block;", "Summarized Data")
         #   div(class = 'homebutton', style="display: inline-block;", "Raw Data")
         #   div(class = 'homebutton', style="display: inline-block;", "Data Entry")    
         # })
         
         #httr::content(login_status, "parsed", "application/json", encoding = "UTF-8")[[3]] 
         
       }

      }

  })

  user_info <- reactive({
    httr::content(login_status, "parsed", encoding = "UTF-8")[[3]]
  })

  output$username <- renderText({
  if(is.null(login_status)){
   NULL 
  } else {
    if(status_code(login_status)!=200){
      NULL
    } else {
      user_info()$Fullname
    }
  }
  })

   #-----------------------------------------------------------------
   #  Dashboard Buttons
   #-----------------------------------------------------------------

# user_info <- content(login_status, "parsed", "application/json", encoding = "UTF-8")[[3]]
# output$username <- renderText({user_info$FullName})


# observeEvent(input$sum_tab,{
#       updateNavbarPage(session, id = "kus_navbar", selected = "sum_data")
#     })
   
   
   
  #-----------------------------------------------------------------
  #  Raw Data
  #-----------------------------------------------------------------
  
  # Generate metadata table to create initial query parameters

    datasets <- reactive({
      if(status_code(login_status) == 200){
      getDatastores(cdms_host = cdms_host) %>%
      mutate(DatastoreDatasetId = as.integer(DatastoreDatasetId)) %>%
      rename(DatasetId = Id, DatasetName = Name) %>%
      arrange(DatasetName)
      }
    })
      
    projects <- reactive({
      if(status_code(login_status) == 200){
      getProjects(cdms_host = cdms_host) %>%
      rename(ProjectName = Name) %>%
      arrange(ProjectName)
      }
    })
  
    locations <- reactive({
      if(status_code(login_status) == 200){      
      getLocations(cdms_host = cdms_host) %>%
      rename(LocationId = Id, LocationName = Name) %>%
      arrange(LocationName)
      }
    })
  
    waterbodies <- reactive({
      if(status_code(login_status) == 200){      
      getWaterbodies(cdms_host = cdms_host) %>%
      rename(WaterBodyId = Id, WaterBodyName = Name) %>%
      arrange(WaterBodyName)
      }
    })
      
    metadata <- reactive({
      if(status_code(login_status) == 200){      
      inner_join(datasets(), locations(), by = c('DatastoreDatasetId' = 'LocationTypeId')) %>%
      inner_join(projects(), by = 'ProjectId') %>%
      inner_join(waterbodies(), by = 'WaterBodyId')
      }
    })
    
  #-----------------------------------------------------------------
  #  Raw Data
  #-----------------------------------------------------------------  
  # build select boxes for raw data query parameters
  
  output$dataset_menu <- renderUI({
    datasetid <- datasets() %>%
      distinct(DatasetId) %>%
      pull()
    
    datasetname <- datasets() %>%
      distinct(DatasetName) %>%
      pull()
    
    datasets_ls <- as.list(datasetid)
    names(datasets_ls) <- datasetname
    selectInput("datasets", h3("Dataset:"), choices = datasets_ls, selectize = TRUE)
  })   

  output$project_menu <- renderUI({
    projectid <- metadata() %>%
      arrange(ProjectName) %>%
      filter(DatasetId == input$datasets) %>%
      distinct(ProjectId) %>%
      pull()
    
    projectname <- metadata() %>%
      arrange(ProjectName) %>%
      filter(DatasetId == input$datasets) %>%
      distinct(ProjectName) %>%
      pull()
    
    project_ls <- as.list(c('NULL', projectid))
    names(project_ls) <- c('All', projectname)
    
    selectInput("projects", h3("Project:"), choices = project_ls, selected = 'All', selectize = TRUE)
  })   
   
   output$waterbody_menu <- renderUI({
     waterbodyid <- metadata() %>%
       arrange(WaterBodyName) %>%
       filter(DatasetId == input$datasets,
              ProjectId == input$projects) %>%
       distinct(WaterBodyId) %>%
       pull()
     
     waterbodyname <- metadata() %>%
       arrange(WaterBodyName) %>%
       filter(DatasetId == input$datasets,
              ProjectId == input$projects) %>%
       distinct(WaterBodyName) %>%
       pull()
     
     waterbody_ls <- as.list(c('NULL', waterbodyid))
     names(waterbody_ls) <- c('All', waterbodyname)
     
     selectInput('waterbody', h3("Stream: Tributary To:"), waterbody_ls, selected = 'All', selectize=TRUE)
   })
   
   output$location_menu <- renderUI({
     locationid <- metadata() %>%
       arrange(LocationName) %>%
       filter(DatasetId == input$datasets,
              ProjectId == input$projects,
              WaterBodyId == input$waterbody) %>%
       distinct(LocationId) %>%
       pull()
     
     locationname <- metadata() %>%
       arrange(LocationName) %>%
       filter(DatasetId == input$datasets,
              ProjectId == input$projects,
              WaterBodyId == input$waterbody) %>%
       distinct(LocationName) %>%
       pull()
     
     location_ls <- as.list(c('NULL', locationid))
     names(location_ls) <- c('All', locationname)
     
     selectInput('location', h3("Location:"), location_ls, selected = 'All', selectize = TRUE)
   })
   
   # get the full dataset view for selected spp, run and survey years
   raw_dat <- eventReactive(input$raw_submit,{
     getDatasetView(datastoreID = input$datasets, projectID = NULL, waterbodyID = input$waterbody, locationID = input$location, cdms_host = cdms_host)
     #getDatasetView(datastoreID = 68, waterbodyID = 1370, locationID = 601, cdms_host = "https://cdms.nptfisheries.org")
     }) 
   
  raw_qry_params <- reactive({ 
    paste0('Created by: Full Name, Date: ', Sys.Date(),
           ', Dataset: ', input$datasets,
           ', Project: ', input$projects,
           ', Stream: ', input$waterbody,
           ', Location: ', input$location) 
  })
 
 
  output$raw_table <- DT::renderDataTable({
    
    tmp_df <- raw_dat()
    DT::datatable(tmp_df, options = list(orderClasses = TRUE), filter = 'top', 
                  caption = paste0('Dataset generated from the Nez Perce Tribes centralized data base managment system
                                   and should be cited accordingly. Query parameters were set as; ', raw_qry_params()))
  })
  
  
  # function for downloading data
  output$raw_export <- downloadHandler(
    filename = function() {
      paste0(input$datasets,"_raw_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(raw_dat(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )  
  
  
  #------------------------------------------------------------------------------
  # Start of summarized section
  #------------------------------------------------------------------------------
  
  output$sum_dataset_menu <- renderUI({
    datasetid <- datasets() %>%
      distinct(DatasetId) %>%
      pull()
    
    datasetname <- datasets() %>%
      distinct(DatasetName) %>%
      pull()
    
    datasets_ls <- as.list(datasetid)
    names(datasets_ls) <- datasetname
    selectInput("sum_datasets", h3("Dataset:"), choices = datasets_ls, selectize = TRUE)
  }) 
  
  sum_dat <- eventReactive(input$sum_submit,{
    getDatasetView(datastoreID = input$sum_datasets, projectID = NULL, waterbodyID = NULL, locationID = NULL, cdms_host = cdms_host)
    #getDatasetView(datastoreID = 68, waterbodyID = 1370, locationID = 601, cdms_host = "https://cdms.nptfisheries.org")
  }) 
  
  sum_qry_params <- reactive({ 
    paste0('Created by: Full Name, Date: ', Sys.Date(),
           ', Dataset: ', input$sum_datasets) 
  })
  
  output$sum_table <- DT::renderDataTable({
    tmp_df <- sum_dat()
    DT::datatable(tmp_df, options = list(orderClasses = TRUE), filter = 'top', 
                  caption = paste0('Dataset generated from the Nez Perce Tribes centralized data base managment system
                                   and should be cited accordingly. Query parameters were set as; ', sum_qry_params()))
  })
  
  
  # function for downloading data
  output$sum_export <- downloadHandler(
    filename = function() {
      paste0(input$datasets,"_summary_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(sum_dat(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$sum_plot <- renderPlot({
    switch(input$sum_datasets,
           
           "68" = sum_dat() %>%
             distinct(ActivityID, .keep_all = TRUE) %>%
             group_by(MPG, POP, TRT_POPID, Species, Run, StreamName, SurveyYear) %>%
             summarise(Redds = sum(NewRedds)) %>%
             ggplot(aes(x = SurveyYear, y = Redds)) +
             geom_line(aes(group=StreamName)) +
             facet_wrap(~TRT_POPID)
             , # redd plots
           
           "69" = plot(10,10) # carcass plots
    )
  })
  
  
  
  #-----------------------------------------------------------------
  # Need to get dataset fields
  #-----------------------------------------------------------------
  
  # output$spp_menu <- renderUI({
  #   radioButtons('spp', h3("Species:"),inline = TRUE, choices = c("Chinook salmon", "Steelhead", "Bull trout"))
  # })
  # 
  # output$run_menu <- renderUI({
  #   radioButtons('run', h3("Run:"), inline = TRUE, choices = c("Spring/summer", "Summer", "Fall"))                                            
  # })
  # 
  # output$year <- renderUI({
  #   sliderInput("year", h3("Survey Year:"), min = 1986, max = year(Sys.Date()),
  #               step = 1, value = c(1986, year(Sys.Date())), sep='')
  # })
  
  #----------------------------------------------------------------
  # second filter layer for exact data request of interest.
  #---------------------------------------------------------------- 
  # mpg_codes <- reactive({
  #   raw_dat() %>%
  #     distinct(MPG) %>%
  #     arrange() %>%
  #     pull()
  # })
  # 
  # output$mpg_menu <- renderUI({
  #   selectInput('mpg', h3("Major Population Groups:"), mpg_codes(), multiple=TRUE, selectize=FALSE, size = 8)
  # })
  # 
  # pop_codes <- reactive({
  #   raw_dat() %>%
  #     filter(MPG %in% input$mpg) %>%
  #     distinct(POP) %>%
  #     arrange() %>%
  #     pull()
  # })
  # 
  # 
  # output$pop_menu <- renderUI({
  #   selectInput('pop', h3("Populations:"), pop_codes(), multiple=TRUE, selectize=FALSE, size = 8)
  # })
  
  #------------------------------------ 
  # Possible use for graphics... 
  # output$field_menu <- renderUI({
  #   fields <- names(raw_dat())
  #   selectInput("fields", h3("Fields:"), choices = fields, selectize = TRUE)
  # }) 
  #---------------------------------------
  
  
  
  # field_values <- reactive({
  #   tmp_data <- input$data
  #   qry <- paste0("SELECT TOP(0) * FROM dbo.", tmp_data)
  #   tmp_names <- names(dbGetQuery(con, qry))
  #   tmp_names %>% discard(~.x %in% c("Survey_ID", "ESU_DPS", "MPG", "POP_NAME", "StreamName", "TributaryTo"))
  # })
  # 
  # output$field_menu <- renderUI({
  #   selectInput('fields', h3("Fields:"), field_values(), multiple = TRUE, selectize = FALSE, size = 8)
  # })
  
  
  
  
  
  
  
  # datasets available should be only those in datastore listed as summarized, or
  # raw data that has been summarized through Shiny
  
    # output$redd_sum_plot <- renderPlot({
    #     validate(need(input$data=="redd_summary", message=FALSE))
    # 
    #   df() %>%
    #     group_by(SpeciesName, Run, StreamName, TributaryTo, SurveyYear) %>%
    #     summarise(total = sum(NewReddCount)) %>%
    #     ggplot(aes(x = as.factor(SurveyYear), y = total,colour = StreamName, group = StreamName)) +
    #     geom_line() +
    #     geom_point() +
    #     theme_bw() +
    #     labs(x = 'Survey Year',
    #          y = 'Total Redds',
    #          title = 'Total redds counted by Nez Perce Tribe during multiple pass surveys'
    #     )
    # })
    # 
    # colorpal <- reactive({
    #   colorFactor("magma", df()$SurveyYears)
    # })
    # 
    # output$redd_detail_plot <- renderLeaflet({
    #   validate(need(input$data=="redd_detail", message=FALSE))
    #   
    #   tmp_redd_detail <- df() %>%
    #     mutate(SurveyYear = as.factor(SurveyYear),
    #            Longitude = as.numeric(Longitude),
    #            Latitude = as.numeric(Latitude))
    #   
    #   long_rng <- range(tmp_redd_detail$Longitude)
    #   lat_rng <- range(tmp_redd_detail$Latitude)
    # 
    #   pal <- colorpal()
    #   
    #   leaflet() %>%
    #     #fitBounds(-117.5, 43, -113, 47.8) %>%
    #     fitBounds(long_rng[1], lat_rng[1], long_rng[2], lat_rng[2]) %>%        
    #     addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
    #     addCircles(data = tmp_redd_detail, lng = ~Longitude, lat = ~Latitude, color = ~pal(SurveyYear),
    #                 fillColor = ~pal(SurveyYear), popup = ~paste(SurveyYear)) #%>%
    #     # addLegend("topright", pal = pal, values = ~tmp_redd_detail$SurveyYear,
    #     #         title = "Survey Year")
    # })
    # 
    # output$carcass_detail_plot <- renderPlot({
    #   validate(need(input$data=="carcass_detail", message=FALSE))
    #   
    #   df() %>%
    #     ggplot(aes(x = as.factor(SurveyYear), fill = Sex)) +
    #     geom_bar(position = "fill") +
    #     theme_bw() +
    #     facet_wrap(~StreamName) +
    #     labs(x = 'Survey Year',
    #          y = 'Sex',
    #          title = 'Proportion of carcasses found during Nez Perce Tribe spawning ground surveys by sex')
    # })   

#-------------------------------------------------------------------------
# gather sgs input values OLD STUFF
#--------------------------------------------------------------------------
# df <- eventReactive(input$submit, {
#   
#   tmp_data <- input$data
#   tmp_spp <- gsub(", ", "', '", toString(input$spp))
#   tmp_run <- gsub(", ", "', '", toString(input$run))
#   tmp_mpg <- gsub(", ", "', '", toString(str_replace(input$mpg, " :.*", "")))
#   tmp_pop <- gsub(", ", "', '", toString(str_replace(input$pop, " :.*", "")))
#   tmp_stream <- gsub(", ", "', '", toString(str_replace(input$stream, " :.*", "")))
# 
#   qry <- paste0("SELECT * FROM dbo.",tmp_data,
#               " WHERE SpeciesName = '",tmp_spp,"' AND Run = '", tmp_run,
#               "' AND SurveyYear BETWEEN ",input$year[1], " AND ", input$year[2],
#               " AND MPG IN('", tmp_mpg, "') AND POP_NAME IN ('", tmp_pop, "') AND StreamName IN('", tmp_stream,"')")
# 
#   dbGetQuery(con, qry)
# })

})
