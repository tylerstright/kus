#------------------------------------------------------------------------------
# Server Logic
# KUS: DFRM Data Summary and Visualization Web Application
# Authors: Ryan N. Kinzer and Tyler Stright
#------------------------------------------------------------------------------
# Load CRAN Packages
library(shiny)
library(tidyverse)
library(httr)
library(plotly)
library(leaflet)

# Load GitHub Packages
library(cdmsR)
#library(cuyem)

# Source External Functions - NEED TO MOVE TO PACKAGE!!!
source('./R/queryRiverData.R')
source('./R/queryWindowCnts.R')
source('./R/summariseSGS.R')
source('./R/summariseRST.R')
source('./R/location_list.R')
source('./R/getSummaryGraph.R')
source('./R/uniqueLocations.R')
source('./R/summariseSGS.R')

#------------------------------------------------------------------------------
# Javascript for "Enter" button
#------------------------------------------------------------------------------

jscode <- '
$(document).keyup(function(event) {
if ((event.keyCode == 13)) {
$("#login").click();
}
});
'
#------------------------------------------------------------------------------
# Load Static Map, Locations and Fish Data 
#------------------------------------------------------------------------------
load('./data/kus_static_map_data.Rdata')
load('./data/redd_df.rda')
load('./data/carcass_df.rda')
load('./data/age_df.rda')
load('./data/abund_df.rda')
load('./data/suv_df.rda')
load('./data/fins_df.rda')

# Transform Static Data
redd_df <- mutate_at(redd_df, .funs = as.numeric, .vars = c('NewRedds', 'Latitude', 'Longitude')) %>%
  mutate(SurveyDate = ymd(str_sub(SurveyDate,1,10)),
         SurveyYear = as.integer(year(SurveyDate)))

#------------------------------------------------------------------------------
# Set Tribal specific variables
#------------------------------------------------------------------------------
cdms_host <- 'https://cdms.nptfisheries.org'
username <- 'api_user'
api_key <- "153054453130053281239582410943958241094537726538860542262540750542640375910349488180619663"
#------------------------------------------------------------------------------
# Login landing page
#------------------------------------------------------------------------------
# Set login variables
login_status <- NULL
makeReactiveBinding("login_status") # for Login Functionality
html_code <- NULL
user_info <- NULL
# Initial login without restricted permissions
startup_status <- cdmsLogin(username, api_key, cdms_host = cdms_host)
html_code <- status_code(startup_status)
user_info <- httr::content(startup_status, "parsed", encoding = "UTF-8")[[3]]

#------------------------------------------------------------------------------
# Gather static session data from CDMS
#------------------------------------------------------------------------------

if(html_code == 200){
  datasets <- getDatastores(cdms_host = cdms_host) %>%
    rename(DatastoreId = Id, DatastoreName = Name)
}

#------------------------------------------------------------------------------
# Define server logic
#------------------------------------------------------------------------------
shinyServer(function(input, output, session) {

#------------------------------------------------------------------------------
# Hide Tabs and Show Tabs
#------------------------------------------------------------------------------
# Hide
  observe({
    hide(selector = "#kus_navbar li a[data-value=tab_rawdata]" )   # raw data
    hide(selector = "#kus_navbar li a[data-value=data_entry]" )    # data entry
  })
  
# Show     

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
  

#------------------------------------------------------------------------------
# Restricted Login
#------------------------------------------------------------------------------
  
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
          footer = "I'm sorry you are having trouble. Try re-checking the post-it note on your computer screen. :)"
        ))
      } else {
        removeModal()
      }
    }
  })
  

#------------------------------------------------------------------
# Homepage figures and data summaries
#------------------------------------------------------------------

  locs <- locations_df$WaterBody %>%
      select(WaterBodyId = Id, StreamName) %>%
      distinct() %>%
  inner_join(locations_df %>%
            select(WaterBodyId, LocationTypeId, Name, Description, Latitude, Longitude) %>%
            distinct(), by = 'WaterBodyId') %>%
  inner_join(locations_df$LocationType %>%
  select(LocationTypeId = Id, LocationTypeName = Name) %>%
    distinct(), by = 'LocationTypeId') %>%
    mutate(LocType = factor(LocationTypeName))
  
  locs_col <- colorFactor(topo.colors(n = n_distinct(locs$LocType)),locs$LocType)
  
  locs_df <- split(locs, locs$LocationTypeName)

  output$home_map <- renderLeaflet({
    
    l <- leaflet() %>%
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
                  color = 'black', weight = 2, opacity = .25, group = 'Nez Perce Reservation')
      
     names(locs_df) %>%
        purrr::walk( function(df) {
          l <<- l %>%
            addCircleMarkers(data=locs_df[[df]],
                       lng=~Longitude, lat=~Latitude,
                       label= ~ Name,
                       popup= ~ paste(StreamName, Name, Description, sep = "<br/>"),
                       group = df,
                       color = 'black', fillColor = locs_col(locs_df[[df]]$LocType), weight = 1, fillOpacity = .5,
                       clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                       labelOptions = labelOptions(noHide = F,
                                                   direction = 'auto'))
        })
        
      l %>%
        addLayersControl(overlayGroups = names(locs_df),
                         options = layersControlOptions(collapsed = TRUE)) %>%
        addLegend("bottomleft", pal = locs_col, values = locs$LocType,
                title = "Data Types",
                opacity = 1
        ) %>%
      addScaleBar(position = 'topright', options = scaleBarOptions())

  })
  
#----------------------------------------------------------------------------
# Summarized Data Tab
#----------------------------------------------------------------------------  
#  Summarized Snake Basin Populaiton Indicators and Metrics
#----------------------------------------------------------------------------
  # # chinook colors
  # ch_copop <- colorFactor(topo.colors(n = n_distinct(ch_pop$POP_NAME)),ch_pop$POP_NAME)
  # # steelhead colors
  # st_copop <- colorFactor(topo.colors(n = n_distinct(st_pop$POP_NAME)),st_pop$POP_NAME)
  # # MPG Map
  # output$MPGmap <- renderLeaflet({
  #   leaflet(options = leafletOptions(minZoom = 6)) %>%
  #     fitBounds(-117.5, 43, -113, 47.8) %>%
  #     setMaxBounds(lng1 = -118.5,
  #                  lat1 = 43,
  #                  lng2 = -112.5,
  #                  lat2 = 47.8) %>%
  #     addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  #     addScaleBar(position = 'topright') 
  # })
  # # Layer MPG polys on radio button select
  # observeEvent(input$mpg_spc, {
  #   if(input$mpg_spc == 'Chinook salmon') {
  #     leafletProxy('MPGmap') %>%
  #       clearGroup('Steelhead Populations') %>%
  #       addPolygons(data = ch_mpg, group = "Chinook Populations", layerId = ch_mpg, fill = FALSE,
  #                   color = 'black', weight = 2, opacity = 1) %>%
  #       addPolygons(data = ch_pop, group = "Chinook Populations", popup = ~as.character(TRT_POPID), layerId = ch_pop$POP_NAME,
  #                   stroke = TRUE, color = 'black', weight = 1, fillOpacity = .5, fillColor = ~ch_copop(POP_NAME)) 
  #   } else {
  #     if(input$mpg_spc == 'Steelhead') {
  #       leafletProxy('MPGmap') %>%
  #         clearGroup('Chinook Populations') %>%
  #         addPolygons(data = st_mpg, group = "Steelhead Populations", layerId = st_mpg, fill = FALSE,
  #                     color = 'black', weight = 2, opacity = 1) %>%
  #         addPolygons(data = st_pop, group = "Steelhead Populations", popup = ~as.character(TRT_POPID), layerId = st_pop$POP_NAME,
  #                     stroke = TRUE, color = 'black', weight = 1, fillOpacity = .5, fillColor = ~st_copop(POP_NAME))
  #     }
  #   }
  # })
  # 
  # # create reactive MPG or POP value for filter
  # values <- reactiveValues(MPG = NULL)
  # # update RST value based on map click (RSTmap)
  # observeEvent(input$MPGmap_shape_click, {
  #   mapclick <- input$MPGmap_shape_click
  #   values$MPG <- mapclick$id
  # })
  # 
  # output$MPGfilter <- renderText({
  #   paste0("POP_NAME filter is: ", values$MPG)
  # })
  
  
#----------------------------------------------------------------------------
# Weir Data
#----------------------------------------------------------------------------
  
  # Weir Location Leaflet Map
  output$weir_map <- renderLeaflet({
    map <- leaflet(options = leafletOptions(minZoom = 7, doubleClickZoom = FALSE)) %>%
      setView(lat = 45.8,
              lng = -116.1,
              zoom = 7) %>%
      setMaxBounds(lng1 = -119,
                   lat1 = 42,
                   lng2 = -114,
                   lat2 = 49) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addAwesomeMarkers(lng= locs_weir$Longitude, lat= locs_weir$Latitude, label= locs_weir$Name,
                        layerId = locs_weir$Name, icon = icon.weir, group = 'Weirs', 
                        labelOptions = labelOptions(noHide = TRUE, direction = 'top')) %>%
      addScaleBar(position = 'topright')
  })
  
  # base weir dataframe (e.g. FINS)
  weir_df <- fins_df %>%
    filter(Species %in% c('Steelhead', 'Chinook')) %>%
    separate(Trap, into = c('Weir', 'Trap'), sep = ' - ') %>%
    separate(`Trapped Date`, into = 'Trapped Date', sep = ' ') %>%
    mutate(Year = year(`Trapped Date`),
           SppRun = paste(`Run`, `Species`)) 
  
  # input$summ_weir
  output$weir_select <- renderUI({
    choose_weir <- c(as.character(sort(unique(c(unique(weir_df$Weir))))))
    selectInput('summ_weir', label = NULL, choices = choose_weir, selectize = FALSE, size = 7, multiple = TRUE)
  })
  
  # input$weiryear
  output$weiryear_select <- renderUI({
    choose_trapyear <- c(as.character(sort(unique(weir_df$Year), decreasing = TRUE)))
    selectInput('weir_year', label =  NULL, choices = choose_trapyear, selectize = FALSE, size = 7, multiple = TRUE)
  })
  
  # Summarise Weir Totals - df / graph
  weirtotals_df <- eventReactive(input$weir_reset, {
    tmp_weirtot  <- fins_df %>%
      filter(Species %in% c('Steelhead', 'Chinook')) %>%
      separate(Trap, into = c('Weir', 'Trap'), sep = ' - ') %>%
      mutate(Year = year(`Trapped Date`),
             SppRun = paste(`Run`, `Species`)) %>%
      group_by(Year, Weir, SppRun) %>%
      summarise(TotalCatch = sum(Count, na.rm = TRUE)) %>%
      #arrange(Weir) %>%
      filter(Weir %in% input$summ_weir,
             Year %in% input$weir_year,
             SppRun == input$weir_spc)
  })
  
  # Total Catch / Year Graph
  output$weir_totals <- renderPlotly({
    if(is.null(input$summ_weir) | is.null(input$weir_year) | is.null(input$weir_spc))
      return()
    ggplotly(ggplot(data= weirtotals_df(), aes(x = Year, y = TotalCatch)) +
               geom_point(aes(color = Weir)) +   
               geom_line(aes(color = Weir), linetype = 1, size = 0.3) +
               theme_bw() +
               scale_color_viridis_d() +
               labs(x = 'Trap Year', 
                    y = 'Total Catch', 
                    title = paste0('Total ', input$weir_spc, ' Catch by Year')))
  })
  
  # Produce Weir Disposition Summary Data
  weirdisp_df <- eventReactive(input$weir_reset,{
    tmp_weir <- weir_df %>%
      group_by(Year, Weir, SppRun, Origin, Sex, Disposition) %>%
      summarise(Count = sum(Count, na.rm = TRUE)) %>%
      spread(key = Disposition, value = Count) %>%
      filter(Weir %in% input$summ_weir,
             Year %in% input$weir_year)
  })
  
  # Disposition Summary Table
  output$weirdisp_table <- DT::renderDataTable({
    weir_tmp <- weirdisp_df()
    DT::datatable(weir_tmp, options = list(orderClasses = TRUE,
                                           autoWidth = TRUE,
                                           dom = 'tpl'),
                  filter = 'top')
  })
  
  # Produce Weir Catch Summary Data
  weircatch_df <- eventReactive(input$weir_reset,{
    tmp_weir1 <- weir_df %>%
      mutate(Origin = case_when(
        Origin %in% c('natural', 'Natural') ~ 'Natural',
        Origin %in% c('Hatchery-Stray', 'hatchery', 'Hatchery', 
                      'supplement', 'Hatchery-Supplementation') ~ 'Hatchery',
        Origin %in% c(NA, 'unknown', 'Unknown', 'Uncategorized') ~ 'Unknown')) %>%
      group_by(Year, Weir, SppRun, Origin, Sex) %>%
      summarise(Count = sum(Count, na.rm = TRUE)) %>%
      spread(key= Sex, value = Count) %>%
      filter(Weir %in% input$summ_weir,
             Year %in% input$weir_year)
  })
  
  # Weir Catch Summary Table
  output$weircatch_table <- DT::renderDataTable({
    weir_tmp1 <- weircatch_df()
    DT::datatable(weir_tmp1, options = list(orderClasses = TRUE,
                                            autoWidth = TRUE,
                                            dom = 'tpl'),
                  filter = 'top')
  })
  
  
#----------------------------------------------------------------------------
#  Summarized SGS Data
#----------------------------------------------------------------------------
  

  # Species Selection Input (All unique species in Carcass and Redd Data)
  output$sgs_species_menu <- renderUI({
    choose_spp <- c('Spring/Summer Chinook', 'Fall Chinook', 'Summer Steelhead', 'Bull Trout')#as.character(sort(unique(redd_df$TargetSpecies)))
    radioButtons("sgs_spp", label = 'Species:', choices = choose_spp, selected = 'Spring/Summer Chinook')
  })
  
  # Year range selection input.
  output$sgs_year_menu <- renderUI({
    choose_year <- range(year(redd_df$SurveyDate))
    sliderInput("sgs_year", label = 'Spawn Year:', min = choose_year[1], max = choose_year[2], value = c(choose_year[1], choose_year[2]), sep='')
  })
  
  # Stream Selection Input (All unique streams in Carcass and Redd Data)
  output$sgs_streams_menu <- renderUI({
    choose_streams <- as.character(sort(unique(c(unique(redd_df$StreamName), unique(carcass_df$StreamName)))))
    selectInput('summ_streams', label = 'Streams:', choices = choose_streams, selectize = FALSE, size =20, multiple = TRUE)
  })
   
  # # Produce SGS Summary data with streams filter APPLIED
  sgs_summary <- eventReactive(input$summ_reset,{
    summariseSGS(redd_df, carcass_df, species = input$sgs_spp, startyear = input$sgs_year[1],
                 endyear = input$sgs_year[2], streams = input$summ_streams)
  })
   
  # # SGS: Redd/Carcass Summary Table Output
  output$summ_table <- DT::renderDataTable({
    tmp_summ <- sgs_summary()[[1]]
    DT::datatable(tmp_summ, options = list(orderClasses = TRUE, autoWidth = TRUE, dom = 'tpl'),
                  filter = 'top')
  })
  
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
  
  
  #-----------------------------------------------------------------
  #  Juvenile Metrics
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
  
  
  #----------------------------------------------------------------------------
  # Hydro-system Conditions and Dam Counts
  #----------------------------------------------------------------------------
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

}) # Close Server
