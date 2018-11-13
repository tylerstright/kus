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
source('./R/getDatasets.R')
source('./R/queryRiverData.R')
source('./R/queryWindowCnts.R')

# Outside Server - Static metadata tables
# Need to set tribal specific variables
cdms_host <- 'https://cdms.nptfisheries.org'
#cdms_host <- 'localhost'
username <- 'api_user'
api_key <- "153054453130053281239582410943958241094537726538860542262540750542640375910349488180619663"

#-----------------------------------------------------------------
# Login landing page
#-----------------------------------------------------------------

login_status <- NULL
html_code <- NULL
user_info <- NULL

login_status <- cdmsLogin(username, api_key, cdms_host = cdms_host)
html_code <- status_code(login_status)
user_info <- httr::content(login_status, "parsed", encoding = "UTF-8")[[3]]

#------------------------------------------------------------------
# Login Modal
#------------------------------------------------------------------
observeEvent(input$openlogin, {
  
})




#------------------------------------------------------------------
# Gather available datasets from CDMS
#------------------------------------------------------------------

if(html_code == 200){
  datasets <- getDatastores(cdms_host = cdms_host) %>%
    rename(DatastoreId = Id, DatastoreName = Name)
}

# Define server logic
shinyServer(function(input, output, session) {

  #------------------------------------------------------------------
  # Homepage Plots and Data
  #------------------------------------------------------------------

   
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
   
   
   
   #----------------------------------------------------------------
   # Pre- and In-Season Management
   #----------------------------------------------------------------
   # Year Selection
   output$year_menu <- renderUI({
     yr <- 2006:year(Sys.Date())
     selectInput('obs_year', 'Migratory Year', choices = yr, selected = year(Sys.Date()), selectize = TRUE)
   })
   
   # Gather river conditions from DART
    flow_df <- eventReactive(input$year_submit, {
      bind_rows(queryRiverData(site = 'LWG', 
                             year = input$obs_year, #year(Sys.Date()),
                             start_day = '01/01',
                             end_day = format(Sys.Date(), '%m/%d')) %>%
                          mutate_all(as.character), 
                    queryRiverData(site = 'BON', 
                                   year = input$obs_year, #year(Sys.Date()),
                                   start_day = '01/01',
                                   end_day = format(Sys.Date(), '%m/%d')) %>%
                      mutate_all(as.character)) %>%
     mutate(Dam = ifelse(Site == 'LWG', 'Lower Granite', 'Bonneville')) %>%
     select(Dam, Site, Date, everything())
    })
   
    # Get possible field names from river data  
   output$river_menu <- renderUI({
     metrics <- sort(names(flow_df()[-(1:3)]))  #c('Inflow', 'Temperature', 'TDG') #

     selectInput('river_metric', 'River Metric (y-axis)', choices = metrics, selected = 'Inflow', selectize = TRUE)
   })
   
   # Plot river conditions
   output$river_plot <- renderPlot({
     flow_df() %>%
       ggplot(aes(x = as.Date(Date), y = as.numeric(!!as.symbol(input$river_metric)))) +
       geom_line(colour = '#536872', size = 1) +
       scale_x_date(date_labels = format('%b-%d')) +
       facet_wrap(~Dam, nrow =1) +
       theme_bw() +
       theme(legend.position = 'none') +
       labs(x = 'Date',
            y = input$river_metric,
            subtitle = paste0('Daily ', input$river_metric, ' at Bonneville and Lower Granite Dam from during ', isolate(input$obs_year),'.'),
            title = 'Mainstem River Conditions'
            )
   })
   
   # Gather window count data
   window_df <- eventReactive(input$year_submit, {
     bind_rows(queryWindowCnts(dam = 'LWG', spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
                            spawn_yr = 2018, start_day = '01/01', end_day = format(Sys.Date(), '%m/%d')) %>%
                 mutate(Site = 'LWG'),
                      queryWindowCnts(dam = 'BON', spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
                                      spawn_yr = 2018, start_day = '01/01', end_day = format(Sys.Date(), '%m/%d')) %>%
                 mutate(Site = 'BON')) %>%
       mutate(Chinook = Chinook + Jack_Chinook,
              Coho = Coho + Jack_Coho,
              Dam = ifelse(Site == 'LWG', 'Lower Granite', 'Bonneville')) %>%
       select(Site, Dam, Date, Chinook, Coho, Steelhead, Wild_Steelhead, Lamprey)
   })
   
   # Get possible species  
   output$spp_menu <- renderUI({
     spp <- c('Chinook', 'Coho', 'Steelhead', 'Wild_Steelhead', 'Lamprey')
     selectInput('window_spp', 'Species', choices = spp, selected = 'Chinook', selectize = TRUE)
   })
   
   # Plot window counts
   output$window_plot <- renderPlot({
     window_df() %>%
       ggplot(aes(x = as.Date(Date), y = as.numeric(!!as.symbol(input$window_spp)))) +
       geom_bar(stat = 'identity') +
       scale_colour_viridis_d() +
       scale_x_date(date_labels = format('%b-%d')) +
       facet_wrap(~Dam, nrow =1) +
       theme_bw() +
       theme(legend.position = 'none') +
       labs(x = 'Date',
            y = input$window_spp,
            subtitle = paste0('Daily window count of ', input$window_spp, ' at Bonneville and Lower Granite Dam during ', isolate(input$obs_year),'.'),
            title = 'Window Counts'
       )
   })
   
  #-----------------------------------------------------------------
  #  Raw Data
  #-----------------------------------------------------------------
   
   output$raw_dataset_menu <- renderUI({
     
     dataset <- datasets %>%
       select(DatastoreId, DatastoreName) %>%
       distinct(DatastoreId, .keep_all = TRUE) %>%
       arrange(DatastoreName)
     
     datasets_ls <- as.list(dataset[,1])
     names(datasets_ls) <- dataset[,2]
     selectInput("datasets", h3("Data Type:"), choices = datasets_ls, selectize = TRUE)
   }) 

   # get the full dataset view for selected spp, run and survey years
   raw_dat <- eventReactive(input$raw_submit,{
     getDatasetView(datastoreID = input$datasets, projectID = NULL, waterbodyID = NULL, locationID = NULL, cdms_host = cdms_host)
     }) 
   
  # raw_qry_params <- eventReactive(input$raw_submit,{
  #   paste0('Created by: ', user_info$Fullname,
  #          ', Date: ', Sys.Date(),
  #          ', Dataset: ', input$datasets,
  #          ', Project: ', input$projects,
  #          ', Stream: ', input$waterbody,
  #          ', Location: ', input$location) 
  # })
 
 
  output$raw_table <- DT::renderDataTable({
    
    tmp_df <- raw_dat()
    DT::datatable(tmp_df, options = list(orderClasses = TRUE), filter = 'top')
    # , 
    #               caption = paste0('Dataset generated from the Nez Perce Tribes centralized data base managment system
    #                                and should be cited accordingly. Query parameters were set as; ', raw_qry_params()))
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
  
})
