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
library(plotly)
library(leaflet)
#devtools::install_github('ryankinzer/cdmsR')
#library(cdmsR)

source('./R/cdmsLogin.R')
source('./R/getDatastores.R')
#source('./R/getHeaderRecords.R')
source('./R/getProjects.R')
#source('./R/getLocations.R')
#source('./R/getWaterbodies.R')
source('./R/getDatasetView.R')
#source('./R/getDatasets.R')
source('./R/queryRiverData.R')
source('./R/queryWindowCnts.R')
source('./R/summariseSGS.R')
source('./R/summariseRST.R')
source('./R/location_list.R')
source('./R/getSummaryGraph.R')
source('./R/uniqueLocations.R')
source('./R/summariseSGS.R')

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
makeReactiveBinding("login_status") # for Login Functionality

html_code <- NULL
user_info <- NULL

startup_status <- cdmsLogin(username, api_key, cdms_host = cdms_host)
html_code <- status_code(startup_status)
user_info <- httr::content(startup_status, "parsed", encoding = "UTF-8")[[3]]
#------------------------------------------
# Gather available datasets from CDMS
#------------------------------------------

if(html_code == 200){
  datasets <- getDatastores(cdms_host = cdms_host) %>%
    rename(DatastoreId = Id, DatastoreName = Name)
}

#------------------------------------------------------------------
# Gather data for homepage
#------------------------------------------------------------------
#redd_df <- getDatasetView(datastoreID = 78, cdms_host = cdms_host)
#save(redd_df, file = './data/redd_df.rda')
#carcass_df <- getDatasetView(datastoreID = 79, cdms_host = cdms_host)
#save(carcass_df, file = './data/carcass_df.rda')
# age_df <- getDatasetView(datastoreID = 80, cdms_host = cdms_host)
# save(age_df, file = './data/age_df.rda')
# abund_df <-gettDatasetView(datastoreID = 85, cdms_host = cdms_host)
# save(abund_df, file = './data/abund_df.rda')
# suv_df <- getDatasetView(datastoreID = 86, cdms_host = cdms_host)
# save(suv_df, file = './data/suv_df.rda')


# Load Data for Summary Tables and Homepage
load('./data/redd_df.rda')
load('./data/carcass_df.rda')
load('./data/age_df.rda')
load('./data/abund_df.rda')
load('./data/suv_df.rda')
load('./data/locations_df.rda')

redd_df <- mutate_at(redd_df, .funs = as.numeric, .vars = c('NewRedds', 'Latitude', 'Longitude')) %>%
  mutate(SurveyDate = ymd(str_sub(SurveyDate,1,10)),
         SurveyYear = as.integer(year(SurveyDate)))

# redd_df <- mutate_at(redd_df, .funs = as.numeric, .vars = c('NewRedds', 'Latitude', 'Longitude')) %>%
# mutate(SppRun = paste0(Species, " - ", Run))

redd_locs <- redd_df %>%
  filter(!is.na(Latitude),
         !is.na(Longitude))

# get map data
load('./data/map_data.Rdata')
load('./data/res_bounds.Rdata')
# # get river flow data
# river_df <- bind_rows(queryRiverData(site = 'LWG',
#                                      year = 2018, #year(Sys.Date()),
#                                      start_day = '01/01',
#                                      end_day = '12/31') %>% #format(Sys.Date(), '%m/%d')) %>%
#                         mutate_all(as.character),
#                       queryRiverData(site = 'BON',
#                                      year = 2018, #year(Sys.Date()),
#                                      start_day = '01/01',
#                                      end_day = '12/31') %>% #format(Sys.Date(), '%m/%d')) %>%
#                         mutate_all(as.character)) %>%
#   mutate(Dam = ifelse(Site == 'LWG', 'Lower Granite', 'Bonneville'),
#          Date = as.Date(Date),
#          Inflow = as.numeric(Inflow)) %>%
# 
#   select(Dam, Site, Date, everything())

# get window count
# win_df <- bind_rows(queryWindowCnts(dam = 'LWG', spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
#                                     spawn_yr = 2018, #year(Sys.Date()),
#                                     start_day = '01/01',
#                                     end_day = '12/31') %>% #format(Sys.Date(), '%m/%d')) %>%
#                       mutate(Site = 'LWG'),
#                     queryWindowCnts(dam = 'BON', spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
#                                     spawn_yr = 2018, #year(Sys.Date()),
#                                     start_day = '01/01',
#                                     end_day = '12/31') %>% #format(Sys.Date(), '%m/%d')) %>%
#                       mutate(Site = 'BON')) %>%
#   mutate(Chinook = Chinook + Jack_Chinook,
#          Coho = Coho + Jack_Coho,
#          Dam = ifelse(Site == 'LWG', 'Lower Granite', 'Bonneville'),
#          Date = as.Date(Date)) %>%
#   select(Site, Dam, Date, Chinook, Coho, Steelhead, Wild_Steelhead, Lamprey)


#------------------------------------------
# Javascript for "Enter" button
#------------------------------------------

jscode <- '
$(document).keyup(function(event) {
if ((event.keyCode == 13)) {
$("#login").click();
}
});
'

# Define server logic
shinyServer(function(input, output, session) {

  
  #------------------------------------------------------------------
  # Hide tabs, Show tabs, Get CDMS datasets
  #------------------------------------------------------------------
  # Hide tabs
  observe({
    hide(selector = "#kus_navbar li a[data-value=tab_rawdata]" )   # raw data
    hide(selector = "#kus_navbar li a[data-value=data_entry]" )    # data entry
  })
  
  # Show Tabs     
  #--------------------------------------------------
  observeEvent(input$login, {
      delay(1000, 
            if(is.null(login_status)) {
              NULL
            } else {
                if(status_code(login_status)==200) {
                  show(selector = "#kus_navbar li a[data-value=tab_rawdata]")
                  show(selector = "#kus_navbar li a[data-value=data_entry]")
                }
            } 
      )
    })
  

  #------------------------------------------------------------------
  # DFRM Login
  #------------------------------------------------------------------
  
  # User information
  user_info <- reactive({
    httr::content(login_status, "parsed", encoding = "UTF-8")[[3]]
  })

  # Create a Login Link that disappears after successful login
   output$log_link <- renderUI({
   if(is.null(login_status)) {
     actionLink('login_link', 'Login Link')
   } else {
     if(status_code(login_status)!=200){
       actionLink('login_link', 'Login Link')
     } else {
         if(status_code(login_status)==200) {
           NULL }
       }
     }
  })
   
  # Display Full Name after successful login
  output$full_name <- renderText({
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
  

  # Login Modal
  observeEvent(input$login_link,
               showModal(modalDialog(
                 textInput('username','Username'), 
                 passwordInput('password', 'Password'), 
                 tags$head(tags$script(HTML(jscode))),
                 actionButton('login', 'Login'),
                 size = "m",
                 easyClose = TRUE,
                 title = "DFRM Fisheries Data Access",
                 footer = "Please contact Clark Watry (clarkw@nezperce.org) to request login credentials."
               ))
  )
  observeEvent(input$login, {
    
    if(input$username == '' | input$password == '')
    {
      showModal(modalDialog(
        textInput('username','Username'),
        passwordInput('password', 'Password'),
        tags$head(tags$script(HTML(jscode))),
        actionButton('login', 'Login'),
        size = "m",
        easyClose = TRUE,
        title = "Username or password fields are blank.",
        footer = "Please fill in your username and password correctly."
      ))
    } else {
      login_status <<- cdmsLogin(input$username, input$password, cdms_host = cdms_host) 
      
      if(status_code(login_status) != 200) {
        showModal(modalDialog(
          textInput('username','Username'),
          passwordInput('password', 'Password'),
          tags$head(tags$script(HTML(jscode))),
          actionButton('login', 'Login'),
          size = "m",
          easyClose = TRUE,
          title = "Invalid Username or Password",
          footer = "I'm sorry you are having trouble. Try re-checking the post-it note on your computer screen."
        ))
      } else {
        removeModal()
      }
    }
  })
  

  #------------------------------------------------------------------
  # Plot homepage figures and data summaries
  #------------------------------------------------------------------

   # Decided on four plots; spatial map of redd counts, redd count trend, window counts, hydrosystem and flow/spill,
   # could also include carcass sex ratios and size
  
  # 1. Spatial redd locations
  # Create List of Dam and Weir Locations:
    Name <- c('Lower Granite Dam', 'Little Goose Dam', 'Lower Monumental Dam',
              'Ice Harbor Dam', 'John Day Dam', 'The Dalles Dam', 'Bonneville Dam', 'McNary Dam')
    Site <- c('LWG', 'LGS', 'LMN', 'IHR', 'JDA', 'TDA', 'BON', 'MCN')
    Latitude <- c(46.661, 46.584, 46.563, 46.25, 45.715, 45.614, 45.645, 45.936)
    Longitude <- c(-117.428, -118.027, -118.538, -118.88, -120.693, -121.133, -121.941, -119.298)
    locs_dam <- tibble(Name, Site, Latitude, Longitude) # Dams
    
    locs_rst <- location_list(data = locations_df, locationtypeId = 1124) # Screw Traps
  
    locs_weir <- location_list(data = locations_df, locationtypeId = 1123) # Weirs
    # Create Icons:
    icon.weir <- makeAwesomeIcon(icon = 'cogs', markerColor = 'black', library='fa',
                               iconColor = 'yellow')
    icon.dam <- makeAwesomeIcon(icon = 'fort-awesome', markerColor = 'red', library='fa',
                                 iconColor = 'black')
    icon.rst <- makeAwesomeIcon(icon = 'cogs', markerColor = 'blue', library='fa',
                                iconColor = 'black') 

  output$redd_map <- renderLeaflet({
    
    pal <- colorFactor(palette = rainbow(3),
                       redd_locs$ReddSpecies)
    
    spp <- as.character(unique(redd_locs$ReddSpecies))
    yr <- as.character(unique(redd_locs$SurveyYear))
    
    map <- leaflet() %>%   #redd_df[redd_df$SurveyYear==year(Sys.Date())-2,]
      setView(lat = 45.65,
              lng = -115.85,
              zoom = 7) %>%
      setMaxBounds(lng1 = -122,
                   lat1 = 42,
                   lng2 = -110,
                   lat2 = 49) %>%
      addProviderTiles(providers$Esri.WorldTopoMap,
                       options = providerTileOptions(minZoom = 6)) %>%
      addPolygons(data = icc, fill = FALSE,
                  color = 'black', weight = 2, opacity = 1, group = '1855 Reservation') %>%
      addPolygons(data = npt1863, fill = 'red',
                  color = 'black', weight = 2, opacity = .25, group = 'Nez Perce Reservation') %>%
      addAwesomeMarkers(lng= locs_weir$Longitude, lat= locs_weir$Latitude, label= locs_weir$Name,
                       layerId = locs_weir$Name, icon = icon.weir, group = 'Weirs') %>%
      addAwesomeMarkers(lng= locs_dam$Longitude, lat= locs_dam$Latitude, label= locs_dam$Name,
                        layerId = locs_dam$Site, icon = icon.dam, group = 'Dams') %>%
                        #popup = popupTable(tmp_window, row.numbers = FALSE, feature.id = 'Daily Counts')),
      addAwesomeMarkers(lng= locs_rst$Longitude, lat= locs_rst$Latitude, label= locs_rst$Name,
                        layerId = locs_rst$Name, icon = icon.rst, group = 'Screw Traps') %>%
      addLayersControl(
        overlayGroups = c('Nez Perce Reservation', '1855 Reservation', 'Dams', 'Weirs', 'Screw Traps'),
        options = layersControlOptions(collapsed = FALSE)) %>%
      addScaleBar(position = 'topright', options = scaleBarOptions())
      
    
    #for(s in spp){
    #   for(y in yr){
    #   d = redd_locs[redd_locs$SurveyYear==y,]
    # 
    #   map = map %>% addCircleMarkers(data = d, lat = ~Latitude, lng = ~Longitude, label = ~WPTName,
    #                        group = y, color = ~pal(ReddSpecies),
    #                        clusterOptions = markerClusterOptions(),
    #                        fillOpacity = .25)
    # #  }
    # }

   #map 
    #%>% 
    #addLayersControl(
    #   overlayGroups = yr,
    #  options = layersControlOptions(collapsed = TRUE)) #%>%
  #    addLegend(pal = pal, values = ~ReddSpecies, title = '', position = 'bottomleft')

  })
  
  # # create reactive DAM value for filter
  # values <- reactiveValues(DAM = NULL)
  # # update RST value based on map click (RSTmap)
  # observeEvent(input$redd_map_marker_click, {
  #   mapclick <- input$redd_map_marker_click
  #   values$DAM <- mapclick$id
  # })
  # 
  # 
  #  tmp_window <- eventReactive(input$redd_map_marker_click, {
  #      tmp_win_df <- win_df %>%
  #       filter(Site == "values$DAM",
  #              Date == '2018-07-08') %>%
  #       select(Chinook, Coho, Steelhead, Wild_Steelhead, Lamprey)
  #    temptable <- DT::datatable(tmp_win_df)
  #  })
  

  # 1. Trend redd counts

  # output$home_redd <- renderPlotly({
  #   
  #   p <- redd_df %>%
  #     distinct(ActivityId, .keep_all = TRUE) %>%
  #     group_by(StreamName, TribToName, ReddSpecies, SurveyYear) %>%
  #     summarise(TotalRedds = sum(NewRedds, na.rm = TRUE)) %>%
  #     ggplot(aes(x = SurveyYear, y = TotalRedds)) +
  #     geom_line(aes(colour = StreamName), size = 1) +
  #     geom_point(aes(colour = StreamName), size = 2) +
  #     scale_colour_viridis_d() +
  #     facet_wrap(~ReddSpecies, ncol = 1, scale = 'free') +
  #     theme_grey() +
  #     labs(x = 'Survey Year',
  #          y = 'Total Redds',
  #          colour = 'Population',
  #          title = '')
  #   
  #   ggplotly(p, height = 700) %>% layout(margin = list(b = 50, l = 90))
  #   
  # })
  
  # 2. River Flow and Spill

  # output$home_river <- renderPlotly({
  #   p <- river_df %>%
  #     ggplot(aes(x = Date)) +
  #     geom_line(aes(y = Inflow), colour = 'darkblue', size = 1) +
  #     scale_x_date(date_labels = format('%b-%d')) +
  #     facet_wrap(~Dam, nrow =1) +
  #     theme_grey() +
  #     labs(x = 'Date',
  #          y = 'Inflow (kcfs)',
  #          title = '')
  # 
  #   ggplotly(p) %>% layout(margin = list(b = 50, l = 90))
  # })

  # 3. Window Counts

  #   output$home_BONwin <- renderPlotly({
  #   p <- win_df %>%
  #     filter(Dam == 'Bonneville') %>%
  #     gather(species, count, Chinook:Lamprey) %>%
  #     ggplot(aes(x = Date, y = count)) +
  #     geom_bar(aes(fill = species), stat = 'identity') +
  #     scale_fill_viridis_d() +
  #     scale_x_date(date_labels = format('%b-%d')) +
  #     facet_grid(species~Dam, scales = 'free_y') +
  #     theme_grey() +
  #     theme(legend.position = 'none') +
  #     labs(x = 'Date',
  #          y = 'Daily Window Count',
  #          title = ''
  #     )
  # 
  #   ggplotly(p) %>% layout(margin = list(b = 50, l = 90))
  # })
  # 
  # output$home_LGRwin <- renderPlotly({
  #   p <- win_df %>%
  #     filter(Dam == 'Lower Granite') %>%
  #     gather(species, count, Chinook:Lamprey) %>%
  #     ggplot(aes(x = Date, y = count)) +
  #     geom_bar(aes(fill = species), stat = 'identity') +
  #     scale_fill_viridis_d() +
  #     scale_x_date(date_labels = format('%b-%d')) +
  #     facet_grid(species~Dam, scales = 'free_y') +
  #     #facet_wrap(~Dam, scales = 'free_x') +
  #     theme_grey() +
  #     theme(legend.position = 'none') +
  #     labs(x = 'Date',
  #          y = 'Daily Window Count',
  #          title = ''
  #     )
  # 
  #   ggplotly(p) %>% layout(margin = list(b = 50, l = 90))
  # })

  #---------------------------------------------------------------
  # Summarized Page
  #---------------------------------------------------------------
  
  
  #----------------------------------------------------------------
  # Hydro-system Conditions and Dam Counts
  #----------------------------------------------------------------
   # Year Selection
   output$year_menu <- renderUI({
     yr <- 2006:year(Sys.Date())
     selectInput('obs_year', 'Migratory Year', choices = yr, selected = year(Sys.Date()), selectize = TRUE)
   })


  
  
   # Gather river conditions from DART
    flow_df <- eventReactive(input$year_submit, {

      bind_rows(queryRiverData(site = 'LWG', 
                             year = input$obs_year,
                             start_day = '01/01',
                             end_day = '12/31') %>%
                          mutate_all(as.character), 
                    queryRiverData(site = 'BON', 
                                   year = input$obs_year,
                                   start_day = '01/01',
                                   end_day = '12/31') %>%
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
   output$river_plot <- renderPlotly({
     ggplotly(flow_df() %>%
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
       )
   })
  
   
   # Gather window count data
   window_df <- eventReactive(input$year_submit, {

     
    bind_rows(queryWindowCnts(dam = 'LWG',
                              spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
                              spawn_yr = input$obs_year,
                              start_day = '01/01',
                              end_day = '12/31') %>%
                          mutate(Site = 'LWG'),
              queryWindowCnts(dam = 'BON',
                              spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
                              spawn_yr = input$obs_year,
                              start_day = '01/01',
                              end_day = '12/31') %>%
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
       ggplot(aes(x = as.Date(Date),
                  y = as.numeric(!!as.symbol(input$window_spp)))) +
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
  
  #-----------------------------------------------------------------
  #  Summarized SGS Data Tab
  #-----------------------------------------------------------------
  # Stream Selection Input (All unique streams in Carcass and Redd Data)
    output$streams_menu <- renderUI({
      choose_streams <- c(as.character(sort(unique(c(unique(redd_df$StreamName), unique(carcass_df$StreamName))))))
      selectInput('summ_streams', label = NULL, choices = choose_streams, selectize = FALSE, size =36, multiple = TRUE) 
  })

  # Produce SGS Summary data with streams filter APPLIED
  sgs_summary <- eventReactive(input$summ_reset,{
    summariseSGS(streamfilter = input$summ_streams, redd_data = redd_df, carcass_data = carcass_df)
  }) 

  # SGS: Redd/Carcass Summary Table Output
  output$summ_table <- DT::renderDataTable({
    tmp_summ <- sgs_summary()[[1]]
    DT::datatable(tmp_summ, options = list(orderClasses = TRUE, autoWidth = TRUE, dom = 'tpl'),
                   filter = 'top')
  })

  #===========================# SGS: Graph outputs
  # Total Redds/Year
  output$sgs1 <- renderPlotly({
    sgs_summary()[[2]]
  })
  # % Females
  output$sgs2 <- renderPlotly({
    sgs_summary()[[3]]
  })
  # pHOS
  output$sgs3 <- renderPlotly({
    sgs_summary()[[4]]
  })
  # Prespawn Mortality
  output$sgs4 <- renderPlotly({
    sgs_summary()[[5]]
  })
  #============================#

  #-----------------------------------------------------------------
  #  JUVENILE METRICS Tab < summariseRST >
  #-----------------------------------------------------------------
  # Create list of Hatchery Release Locations
  # locs_rel <- uniqueLocations() %>%
  #   filter(grepl('Survival', type))
  # RST Leaflet Map
  output$RSTmap <- renderLeaflet({
    map <- leaflet(options = leafletOptions(minZoom = 8, maxZoom = 8, 
                                             zoomControl = FALSE, 
                                             dragging = FALSE,
                                             doubleClickZoom = FALSE)) %>%
      setView(lat = 45.5,
              lng = -116.1,
              zoom = 8) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addCircleMarkers(lng= locs_rst$Longitude, lat= locs_rst$Latitude, label= locs_rst$Name, radius = 8,
                       color = 'black', layerId = locs_rst$Name, group = 'Screw Traps') %>%
      # addCircleMarkers(lng= locs_rel$Longitude, lat= locs_rel$Latitude, label= locs_rel$Name, radius = 8,
      #                  color = 'green', layerId = locs_rel$Name, group = 'Screw Traps') %>%
      addScaleBar(position = 'topright')
  })
  
  # create reactive RST value for filter
  values <- reactiveValues(RST = NULL)
  # update RST value based on map click (RSTmap)
  observeEvent(input$RSTmap_marker_click, {
    mapclick <- input$RSTmap_marker_click
    values$RST <- mapclick$id
  })
  

  # Summary Graph Output - Abundance and Survival
  output$juv_sum1 <- renderPlotly({
    if(is.null(values$RST))
      return()
  getSummaryGraph(data = abund_df, rstfilter = values$RST, yaxis = ~Abundance) #%>%
      # layout(title = paste0('Juvenile Abundance and Survival Estimates to Lower Granite Dam for ',
      #                       word(values$RST, sep = ' Rotary Screw Trap'))) %>%
      # layout(legend = list(orientation = 'h', xanchor = 'center', x = 0.5, y = -0.15),
      #        xaxis = list(title = 'Year'),
      #        yaxis = list(title = 'Abundance'),
      #        xaxis2 = list(title = 'Year'),
      #        yaxis2 = list(title = 'Survival', side = 'right'))
  })
  
  output$juv_sum2 <- renderPlotly({
    if(is.null(values$RST))
      return()
    getSummaryGraph(data= suv_df, rstfilter = values$RST, yaxis = ~Survival) 
  })
  
  # Produce Juvenile Summary data with values$RST filter APPLIED
  rstsumm_df <- eventReactive(values$RST,{
    summariseRST(rstfilter = values$RST, rst_data = abund_df, suv_data = suv_df)
  }) 
    
  # Summary Table Output
  output$rstsumm_table <- DT::renderDataTable({
    rst_tmp <- rstsumm_df()
    DT::datatable(rst_tmp, options = list(orderClasses = TRUE, 
                                          autoWidth = TRUE, 
                                          dom = 'tpl'),
                  filter = 'top') 
  })
  
  #-----------------------------------------------------------------
  #  Summarized Snake Basin Populaiton Indicators and Metrics
  #-----------------------------------------------------------------
  # chinook colors
  ch_copop <- colorFactor(topo.colors(n = n_distinct(ch_pop$POP_NAME)),ch_pop$POP_NAME)
  # steelhead colors
  st_copop <- colorFactor(topo.colors(n = n_distinct(st_pop$POP_NAME)),st_pop$POP_NAME)
  # MPG Map
  output$MPGmap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6)) %>%
      fitBounds(-117.5, 43, -113, 47.8) %>%
      setMaxBounds(lng1 = -118.5,
                   lat1 = 43,
                   lng2 = -112.5,
                   lat2 = 47.8) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addScaleBar(position = 'topright') 
    })
  # Layer MPG polys on radio button select
  observeEvent(input$mpg_spc, {
    if(input$mpg_spc == 'Chinook salmon') {
    leafletProxy('MPGmap') %>%
        clearGroup('Steelhead Populations') %>%
        addPolygons(data = ch_mpg, group = "Chinook Populations", layerId = ch_mpg, fill = FALSE,
                    color = 'black', weight = 2, opacity = 1) %>%
        addPolygons(data = ch_pop, group = "Chinook Populations", popup = ~as.character(TRT_POPID), layerId = ch_pop$POP_NAME,
                    stroke = TRUE, color = 'black', weight = 1, fillOpacity = .5, fillColor = ~ch_copop(POP_NAME)) 
    } else {
      if(input$mpg_spc == 'Steelhead') {
    leafletProxy('MPGmap') %>%
        clearGroup('Chinook Populations') %>%
        addPolygons(data = st_mpg, group = "Steelhead Populations", layerId = st_mpg, fill = FALSE,
                    color = 'black', weight = 2, opacity = 1) %>%
        addPolygons(data = st_pop, group = "Steelhead Populations", popup = ~as.character(TRT_POPID), layerId = st_pop$POP_NAME,
                    stroke = TRUE, color = 'black', weight = 1, fillOpacity = .5, fillColor = ~st_copop(POP_NAME))
      }
    }
  })
  
  # create reactive MPG or POP value for filter
  values <- reactiveValues(MPG = NULL)
  # update RST value based on map click (RSTmap)
  observeEvent(input$MPGmap_shape_click, {
    mapclick <- input$MPGmap_shape_click
    values$MPG <- mapclick$id
  })
  
  output$MPGfilter <- renderText({
    paste0("POP_NAME filter is: ", values$MPG)
  })
    
}) # Close Server
