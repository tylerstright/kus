# KUS: DFRM Data Summary and Visualization Web Application: Server Logic
# Authors: Ryan N. Kinzer and Tyler Stright

# Load Packages ----
  # CRAN
library(shiny)
library(tidyverse)
library(httr)
library(plotly)
library(leaflet)
library(sf)
  # GitHub
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

# Javascript for "Enter" button
jscode <- '
$(document).keyup(function(event) {
if ((event.keyCode == 13)) {
$("#login").click();
}
});
'
# Load Static Map, Locations and Fish Data and any trasformations ----
load('./data/kus_static_map_data.Rdata')
#save.image('./data/kus_static_map_data.Rdata')

# Chinook population colors
ch_copop <- colorFactor(topo.colors(n = n_distinct(ch_pop$POP_NAME)),ch_pop$POP_NAME)
# Steelhead population colors
st_copop <- colorFactor(topo.colors(n = n_distinct(st_pop$POP_NAME)),st_pop$POP_NAME)
# set map icon
fish <- makeAwesomeIcon(icon= 'flag', markerColor = 'green', iconColor = 'white', library = "fa")

# Transform Static Data
redd_df <- mutate_at(redd_df, .funs = as.numeric, .vars = c('NewRedds', 'Latitude', 'Longitude')) %>%
  mutate(SurveyDate = ymd(str_sub(SurveyDate,1,10)),
         SurveyYear = as.integer(year(SurveyDate)))

# base weir dataframe (e.g. FINS)
fins_df <- fins_df %>%
  separate(Trap, into = c('Weir', 'Trap'), sep = ' - ') %>%      
  mutate(Year = year(`Trapped Date`),
         Run = case_when(
           Species == 'Chinook' & Run == 'Spring' ~ 'Spring/Summer',
           Species == 'Chinook' & Run == 'Summer' ~ 'Spring/Summer',
           TRUE ~ Run),
         Origin = str_to_title(Origin),
         Origin = case_when(
           Origin == 'Supplement' ~ 'Hatchery-Supplementation',
           Origin == 'Native' ~ 'Natural',
           TRUE ~ Origin),
         SppRun = case_when(
           Species == 'Chinook' ~ paste(Run, Species),
           Species == 'Steelhead' ~ paste(Run, Species),
           TRUE ~ Species))

# Set variables ----
  # Tribal Specific
cdms_host <- 'https://cdms.nptfisheries.org'
username <- 'api_user'
api_key <- "153054453130053281239582410943958241094537726538860542262540750542640375910349488180619663"
  # Login
login_status <- NULL
makeReactiveBinding("login_status")
html_code <- NULL
user_info <- NULL
  # Initial login without restricted permissions
startup_status <- cdmsLogin(username, api_key, cdms_host = cdms_host)
html_code <- status_code(startup_status)
user_info <- httr::content(startup_status, "parsed", encoding = "UTF-8")[[3]]

# Gather static session data from CDMS----
if(html_code == 200){
  datasets <- getDatastores(cdms_host = cdms_host) %>%
    rename(DatastoreId = Id, DatastoreName = Name)
}

# Start of Server ----
shinyServer(function(input, output, session) {
# Hide & show Tabs ----
  observe({
    if(is.null(login_status)) {
    #hideElement(selector = "#kus_navbar li a[data-value=tab_reports]" )  
    hideElement(selector = "#kus_navbar li a[data-value=tab_rawdata]" ) 
    hideElement(selector = "#kus_navbar li a[data-value=data_entry]" )
    } else {
      if(status_code(login_status) != 200) {
        #hideElement(selector = "#kus_navbar li a[data-value=tab_reports]" )  
        hideElement(selector = "#kus_navbar li a[data-value=tab_rawdata]" ) 
        hideElement(selector = "#kus_navbar li a[data-value=data_entry]" )
      } else {
          #showElement(selector = "#kus_navbar li a[data-value=tab_reports]")
          showElement(selector = "#kus_navbar li a[data-value=tab_rawdata]")
          showElement(selector = "#kus_navbar li a[data-value=data_entry]")
      }
    }
  })
  
# Restricted Login and Logout button ----

  # User information
  user_info <- reactive({
    httr::content(login_status, "parsed", encoding = "UTF-8")[[3]]
  })

  # Create Login / Logout Functionality
   output$login_logout <- renderUI({
   if(is.null(login_status)) {
     actionLink('login_link', 'Sign In', icon = icon('sign-in-alt'))
   } else {
     if(status_code(login_status) == 200) {
      actionLink('logout_link', label = paste(user_info()$Fullname, ' [Sign Out]'),
                 icon = icon('sign-out-alt'))
   } else {
     actionLink('login_link', 'Sign In', icon = icon('sign-in-alt'))
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
          #showElement(selector = "#kus_navbar li a[data-value=tab_reports]")
          showElement(selector = "#kus_navbar li a[data-value=tab_rawdata]")
          showElement(selector = "#kus_navbar li a[data-value=data_entry]")
          output$login_logout <- renderUI({
            actionLink('logout_link', label = paste(user_info()$Fullname, ' [Sign Out]'),
                       icon = icon('sign-out-alt'))
             })
          }
      }
  })
  
  # Logout - reset to startup values
    observeEvent(input$logout_link, {
        login_status <<- NULL
       # hideElement(selector = "#kus_navbar li a[data-value=tab_reports]") 
        hideElement(selector = "#kus_navbar li a[data-value=tab_rawdata]")
        hideElement(selector = "#kus_navbar li a[data-value=data_entry]")
        output$login_logout <- renderUI({actionLink('login_link', 'Sign In', icon = icon('sign-in-alt'))}) 
      })

# Kus Home Tab ----

  output$home_map <- renderLeaflet({
    
    locs_col <- colorFactor(topo.colors(n = n_distinct(locs$LocType)),locs$LocType)
    
    locs_df <- split(locs, locs$LocationTypeName)
    
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
  
# Summarized Data Menu ----
    
  #//////////////////////////////////////////////////////////////////////////////
  # Snake Basin Population Tab ----
  #//////////////////////////////////////////////////////////////////////////////
  # Pop species
  output$pop_spp_menu <- renderUI({
    choose_spp <- c('Spring/Summer Chinook', 'Fall Chinook', 'Summer Steelhead')
    selectInput(inputId = 'pop_spp', label = 'Species:', choices = choose_spp, selectize = FALSE, size = 3, multiple = FALSE)
  })
  
  # Pop Site
  output$pop_menu <- renderUI({
    choose_weir <- sort(unique(c(ch_pop$TRT_POPID,st_pop$TRT_POPID)))
    selectInput('weir_site', label = 'Trap Site:', choices = choose_weir, selectize = FALSE, size = 5, multiple = TRUE)
  })
  
  # Pop Year
  output$pop_year_menu <- renderUI({
    choose_year <- as.integer(c(1900,1950))
    sliderInput("weir_year", label = 'Spawn Year:', min = choose_year[1], max = choose_year[2], value = c(choose_year[1], choose_year[2]), sep='')
  })
  
  # Pop Map
  output$pop_map <- renderLeaflet({
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
  observeEvent(input$pop_spp, {
    if(input$pop_spp == 'Spring/Summer Chinook') {
      leafletProxy('pop_map') %>%
        clearGroup('Summer Steelhead') %>%
        addPolygons(data = ch_mpg, group = "Spring/Summer Chinook", layerId = ch_mpg, fill = FALSE,
                    color = 'black', weight = 2, opacity = 1) %>%
        addPolygons(data = ch_pop, group = "Spring/Summer Chinook", popup = ~as.character(TRT_POPID), layerId = ch_pop$POP_NAME,
                    stroke = TRUE, color = 'black', weight = 1, fillOpacity = .5, fillColor = ~ch_copop(POP_NAME))
    } else {
      if(input$pop_spp == 'Summer Steelhead') {
        leafletProxy('pop_map') %>%
          clearGroup('Spring/Summer Chinook') %>%
          addPolygons(data = st_mpg, group = 'Summer Steelhead', layerId = st_mpg, fill = FALSE,
                      color = 'black', weight = 2, opacity = 1) %>%
          addPolygons(data = st_pop, group = 'Summer Steelhead', popup = ~as.character(TRT_POPID), layerId = st_pop$POP_NAME,
                      stroke = TRUE, color = 'black', weight = 1, fillOpacity = .5, fillColor = ~st_copop(POP_NAME))
      }
    }
  })

  # create reactive MPG value for filter
  values <- reactiveValues(MPG = NULL)
  # update MPG value based on pop_map click
  observeEvent(input$pop_map_shape_click, {
    mapclick <- input$pop_map_shape_click
    values$MPG <- mapclick$id
  })

  # MPG Filter Test
  output$MPGfilter <- renderText({
    paste0("POP_NAME filter is: ", values$MPG)
  })
  
  
  #//////////////////////////////////////////////////////////////////////////////
  # Adult Weir Tab ----
  #//////////////////////////////////////////////////////////////////////////////
  
  # Weir species
  output$weir_spp_menu <- renderUI({
    choose_spp <- sort(unique(fins_df$SppRun)) #c('Fall Chinook', 'Spring Chinook', 'Summer Chinook', 'Summer Steelhead')
  selectInput(inputId = 'weir_spp', label = 'Species:', choices = choose_spp, selectize = FALSE, size = 5, multiple = TRUE)
  })
  # Weir Site
  output$weir_menu <- renderUI({
    choose_weir <- sort(unique(fins_df$Weir))
    selectInput('weir_site', label = 'Trap Site:', choices = choose_weir, selectize = FALSE, size = 5, multiple = TRUE)
  })
  # Weir Year
  output$weir_year_menu <- renderUI({
    choose_year <- range(fins_df$Year)
    sliderInput("weir_year", label = 'Spawn Year:', min = choose_year[1], max = choose_year[2], value = c(choose_year[1], choose_year[2]), sep='')
  })
  # Weir Location Leaflet Map
  output$weir_map <- renderLeaflet({
    
    locs_datatype <- locs %>% filter(LocationTypeName == 'Adult Weir')

    l <- leaflet(options = leafletOptions(minZoom = 7, doubleClickZoom = FALSE)) %>%
      setView(lat = 45.8,
              lng = -116.1,
              zoom = 7) %>%
      setMaxBounds(lng1 = -119,
                   lat1 = 42,
                   lng2 = -114,
                   lat2 = 49) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addAwesomeMarkers(lng= locs_datatype$Longitude, lat= locs_datatype$Latitude, label= locs_datatype$Name,
                        layerId = locs_datatype$Name, icon = fish, group = 'Weirs', 
                        labelOptions = labelOptions(noHide = FALSE, direction = 'top')) %>%
      addScaleBar(position = 'topright')
  })
  
  # Summarise Weir Counts
  weir_df <- eventReactive(input$weir_button, {
    fins_df %>%
      filter(SppRun %in% input$weir_spp) %>%
      filter(Weir %in% input$weir_site) %>%
      filter(Year >= input$weir_year[1]) %>%
      filter(Year <= input$weir_year[2]) %>%
    group_by(Year, Weir, SppRun, Origin, Sex, Disposition) %>%
      summarise(Count = sum(Count, na.rm = TRUE))
  })

  # Total Catch/Year Graph - weir_totals
  output$weir_totals <- renderPlot({
   if(is.null(input$weir_site) | is.null(input$weir_year[1]) | is.null(input$weir_spp))
     return()
    
   weir_df() %>%
      group_by(Year, Weir, SppRun, Origin, Sex) %>%
      summarise(Trapped = sum(Count, na.rm=TRUE)) %>%
      ggplot(aes(x = Year, y = Trapped)) +
               geom_point(aes(colour = Weir)) +
               geom_line(aes(color = Weir, linetype = Sex), size = 0.3) +
               theme_bw() +
               scale_color_viridis_d() +
               facet_wrap(~ Origin, ncol = 2) +
               labs(x = 'Trap Year',
                    y = 'Total Catch',
                    colour = 'Weir',
                    linetype = 'Sex',
                    title = paste0('Total ', input$weir_spp, ' Catch by Year'))

  })
  
  # Disposition Summary - weir_table
  output$weir_table <- DT::renderDataTable({
    
    weir_tmp <- weir_df() %>%
      spread(key = Disposition, value = Count)
    
    DT::datatable(weir_tmp, options = list(orderClasses = TRUE,
                                           autoWidth = TRUE,
                                           dom = 'tpl'),
                  filter = 'top')
  })
  
  #//////////////////////////////////////////////////////////////////////////////
  # In-Stream Abundance Tab ----
  #//////////////////////////////////////////////////////////////////////////////
  
  # Load DABOM Stuff
  load('./data/DABOM_map_data.rda')
  site_meta <- read_csv('./data/dabom_site_metadata.csv')
  
  CR_sites <- CR_sites %>%
    left_join(site_meta, by =  'SiteID') %>%
    mutate(DABOMsite = case_when(
      DABOMsite == TRUE ~ TRUE,
      DABOMsite == FALSE ~ FALSE,
      is.na(DABOMsite) ~ FALSE)) %>%
    select(contains('Site'), operations, firstYearO, lastYearOp, Basin,
           GRA_Locati, Area, Branch, Funding, SitePurpose, PreFastTrack, contains('DABOM'),
           ManagementCritical, `PTAGISO&M`, `O&M_Agency`, OperationalTime,
           `CH_POP_coverage%`, ChinookGSI, `ST_POP_coverage%`, SteelheadGSI, Latitude, Longitude, geometry)
  # %>%
  #   select(-contains('Trib_'))
  
  # DABOM Basin
  output$dabom_basin <- renderUI({
    choose_basin <- unique(CR_sites$Basin) # shiny select box
    selectInput("dabom_basin", label = 'Basin:', choices = choose_basin, selectize = FALSE, size = 5, multiple = TRUE)
  })
  # DABOM Site Type
  output$dabom_sitetype <- renderUI({
    choose_type <- unique(CR_sites$SiteType) # shiny select box
    selectInput("dabom_type", label = 'Site Type:', choices = choose_type, selectize = FALSE, size = 5, multiple = TRUE)
  })
  # DABOM Site Type Name
  output$dabom_sitetypename <- renderUI({
    choose_typename <- unique(CR_sites$SiteTypeNa) # shiny select box
    selectInput("dabom_typename", label = 'Site Type Name:', choices = choose_typename, selectize = FALSE, size = 5, multiple = TRUE)
  })
  # DABOM Model Sites
  output$dabom_modelsites <- renderUI({
    choose_modelsite <- unique(CR_sites$DABOMsite)
    selectInput("dabom_modelsite", label = 'DABOM Model Site:', choices = choose_modelsite, selectize = FALSE, size = 2, multiple = TRUE)
    
  })
  # DABOM Species
  output$dabom_spp <- renderUI({
    choose_spp <- c('Chinook', 'Steelhead') # shiny select box
    selectInput("dabom_spp", label = 'Species:', choices = choose_spp, selectize = FALSE, size = 1, multiple = FALSE)
  })
  # DABOM MPG  
  pops <- eventReactive(input$dabom_spp, {
    if(input$dabom_spp == 'Chinook'){
      return(SR_ch_pop)
    }
    if(input$dabom_spp == 'Steelhead'){
      return(SR_st_pop)
    }
  })

  output$dabom_mpg <- renderUI({
    choose_mpgs = unique(pops()$MPG) # shiny select box
    selectInput("dabom_mpgs", label = 'Major Population Groups:', choices = choose_mpgs, selectize = FALSE, size = 7, multiple = TRUE)
  })
  
  # DABOM Export data
  output$dabom_export <- downloadHandler(
    filename = function() {
      paste0("dabom_site_config", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(CR_sites, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # DABOM Map
  output$dabom_map <- renderLeaflet({

    leaflet(options = leafletOptions(minZoom = 7, doubleClickZoom = FALSE)) %>%
      setView(lat = 45.8,
              lng = -116.1,
              zoom = 7) %>%
      setMaxBounds(lng1 = -119,
                   lat1 = 42,
                   lng2 = -114,
                   lat2 = 49) %>%
      #addProviderTiles(providers$OpenStreetMap)
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addPolylines(data = CR_streams, weight = 2) %>%
      addScaleBar(position = 'topright')
    
  })
  
  map_sites <- eventReactive(input$dabom_btn, {
    CR_sites %>%
      filter(SiteType %in% input$dabom_type,
             SiteTypeNa %in% input$dabom_typename,
             DABOMsite %in% input$dabom_modelsite,
             Basin %in% input$dabom_basin)
  })
  
  
  observeEvent(input$dabom_btn,{

    map_pops <- pops() %>%
      filter(MPG %in% input$dabom_mpgs)

    copop <- colorFactor(topo.colors(n = n_distinct(map_pops$POP_NAME)),map_pops$POP_NAME)
    
    leafletProxy("dabom_map") %>%
      clearMarkers() %>%
      clearGroup("polys") %>%
      clearGroup("sites") %>%
      addPolygons(data = map_pops, group = "polys",
                  popup = ~as.character(TRT_POPID), layerId = map_pops$POP_NAME,
                  stroke = TRUE, color = 'black', weight = 1, fillOpacity = .5,
                  fillColor = ~copop(POP_NAME)) %>%  
      addCircles(map_sites()$Longitude, map_sites()$Latitude, group = 'sites', radius = 15,
                 color = 'red', 
                 popup = paste("<b>Site ID:</b>", map_sites()$SiteID, "<br>",
                               "<b>Site Type:</b>", map_sites()$SiteType, "<br>",
                               "<b>Site Name:</b>", map_sites()$SiteName, "<br>",
                               "<b>DABOM Site:</b>", map_sites()$DABOMsite),
                 popupOptions = popupOptions(noHide = T, textsize = "15px"),
                 highlightOptions = highlightOptions(color = "white",
                                                     weight = 5, bringToFront = F, opacity = 1))
  })
  
  # DABOM Data Table  
    output$x1 = DT::renderDT(CR_sites, selection = 'none', editable = TRUE)
    
    proxy = DT::dataTableProxy('x1')
    
    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      CR_sites[i, j] <<- DT::coerceValue(v, CR_sites[i, j])
      DT::replaceData(proxy, CR_sites, resetPaging = FALSE)  # important
    })
    
    #output$site_table <- DT::renderDT({    
    # site_tmp <- map_sites() %>%
    #   select(contains('Site'))
    # 
    # DT::datatable(site_tmp, options = list(orderClasses = TRUE,
    #                                        autoWidth = TRUE,
    #                                        dom = 'tpl'),
    #               filter = 'top')
  #})
  
  
  #//////////////////////////////////////////////////////////////////////////////
  # SGS Tab ----
  #//////////////////////////////////////////////////////////////////////////////
  
  # Species Selection
  output$sgs_species_menu <- renderUI({
    choose_spp <- c('Spring/Summer Chinook', 'Fall Chinook', 'Summer Steelhead', 'Bull Trout')
    selectInput("sgs_spp", label = 'Species:', choices = choose_spp, selectize = FALSE, size = 5, multiple = TRUE)
  })
  # Stream Selection
  output$sgs_streams_menu <- renderUI({
    choose_streams <- sort(unique(c(redd_df$StreamName, carcass_df$StreamName)))
    selectInput('summ_streams', label = 'Streams:', choices = choose_streams, selectize = FALSE, size =5, multiple = TRUE)
  })
  # Year range selection
  output$sgs_year_menu <- renderUI({
    choose_year <- range(year(redd_df$SurveyDate))
    sliderInput("sgs_year", label = 'Spawn Year:', min = choose_year[1], max = choose_year[2], value = c(choose_year[1], choose_year[2]), sep='')
  })
  
  # SGS Map
  output$sgs_map <- renderLeaflet({
    
    locs_datatype <- locs %>% filter(LocationTypeName == 'Spawning Ground Survey')

    l <- leaflet(options = leafletOptions(minZoom = 7, doubleClickZoom = FALSE)) %>%
      setView(lat = 45.8,
              lng = -116.1,
              zoom = 7) %>%
      setMaxBounds(lng1 = -119,
                   lat1 = 42,
                   lng2 = -114,
                   lat2 = 49) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addAwesomeMarkers(lng= locs_datatype$Longitude, lat= locs_datatype$Latitude, label= locs_datatype$Name,
                        layerId = locs_datatype$Name, icon = fish, group = 'Weirs', 
                        labelOptions = labelOptions(noHide = FALSE, direction = 'top')) %>%
      addScaleBar(position = 'topright')
  })

  # # Produce SGS Summary data
  sgs_summary <- eventReactive(input$summ_reset,{
    summariseSGS(redd_df, carcass_df, species = input$sgs_spp, startyear = input$sgs_year[1],
                 endyear = input$sgs_year[2], streams = input$summ_streams)
  })
   
  # SGS PLOTS:
  # sgs1 - Total Redds/Year
  output$sgs1 <- renderPlotly({
    sgs_summary()[[2]]
  })
  # sgs2 - % Females
  output$sgs2 <- renderPlotly({
    sgs_summary()[[3]]
  })
  # sgs3 - pHOS
  output$sgs3 <- renderPlotly({
    sgs_summary()[[4]]
  })
  # sgs4 - Prespawn Mortality
  output$sgs4 <- renderPlotly({
    sgs_summary()[[5]]
  })
  
  # SGS Summary Table
  output$summ_table <- DT::renderDataTable({
    tmp_summ <- sgs_summary()[[1]]
    DT::datatable(tmp_summ, options = list(orderClasses = TRUE, autoWidth = TRUE, dom = 'tpl'),
                  filter = 'top')
  })
  
  #//////////////////////////////////////////////////////////////////////////////
  # Juvenile Tab
  #//////////////////////////////////////////////////////////////////////////////
  # Species Selection
  output$juv_species_menu <- renderUI({
    choose_spp <- c('Spring/Summer Chinook', 'Fall Chinook', 'Summer Steelhead' )
    selectInput("juv_spp", label = 'Species:', choices = choose_spp, selectize = FALSE, size = 3, multiple = FALSE)
  })
  # Stream Selection
  output$juv_streams_menu <- renderUI({
    choose_streams <- sort(unique(c(abund_df$Location, suv_df$Location)))
    selectInput('juv_locations', label = 'Streams:', choices = choose_streams, selectize = FALSE, size =5, multiple = FALSE)
  })
  # Year range selection
  output$juv_year_menu <- renderUI({
    choose_year <- range(abund_df$MigratoryYear, suv_df$MigratoryYear)
    sliderInput("juv_year", label = 'Migratory Year:', min = choose_year[1], max = choose_year[2], step = 1, value = c(choose_year[1], choose_year[2]), sep='')
  })
  
  # Juvenile Map
  output$juv_map <- renderLeaflet({
    
    locs_datatype <- locs %>% filter(LocationTypeName %in% c('Rotary Screw Trap', 'Juvenile Survival'))
    
    l <- leaflet(options = leafletOptions(minZoom = 7, doubleClickZoom = FALSE)) %>%
      setView(lat = 45.8,
              lng = -116.1,
              zoom = 7) %>%
      setMaxBounds(lng1 = -119,
                   lat1 = 42,
                   lng2 = -114,
                   lat2 = 49) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addAwesomeMarkers(lng= locs_datatype$Longitude, lat= locs_datatype$Latitude, label= locs_datatype$Name,
                        layerId = locs_datatype$Name, icon = fish, group = 'Weirs', 
                        labelOptions = labelOptions(noHide = FALSE, direction = 'top')) %>%
      addScaleBar(position = 'topright')
  })
  
  # Produce Juvenile Summary data
  juv_df <- eventReactive(input$juv_reset,{
    summariseRST(abund_df, suv_df, input$juv_spp, input$juv_year[1], input$juv_year[2],input$juv_locations)
  }) 

  # juv_sum1 - Abundance
  output$juv_sum1 <- renderPlotly({
    if(is.null(input$juv_reset))
      return()
    getSummaryGraph(data= juv_df(), metric = ~Abundance) 
    
    # tmp_df <- juv_df() %>%
    #   mutate(cohort = paste0(Origin, ' Origin ',Species)) %>%
    #   filter(Lifestage == 'Total')
    # 
    # plot_ly(data = tmp_df, 
    #              x = ~MigratoryYear, 
    #              y = ~Abundance, 
    #              type = 'scatter',
    #              mode = 'lines+markers',
    #              color = ~cohort,
    #              colors = viridis_pal(option = "D")(8),
    #              text = ~paste(cohort)) %>%
    #   layout(legend = list(x = 0, y = -0.15, orientation = 'h'))

  })
  
  # juv_sum2 - Survival
  output$juv_sum2 <- renderPlotly({
    if(is.null(input$juv_reset))
      return()
    getSummaryGraph(data= juv_df(), metric = ~Survival) 
    # 
    # tmp_df <- tmp %>% #juv_df() %>%
    #   mutate(cohort = paste0(Origin, ' Origin ',Species)) %>%
    #   filter(Lifestage == 'Smolt') %>%
    #   arrange(MigratoryYear)
    # 
    # plot_ly(data = tmp_df, 
    #         x = ~MigratoryYear, 
    #         y = ~Survival, 
    #         type = 'scatter',
    #         mode = 'lines+markers',
    #         color = ~cohort,
    #         colors = viridis_pal(option = "D")(8),
    #         text = ~paste(cohort)) %>%
    #   layout(legend = list(x = 0, y = -0.15, orientation = 'h'))
  })

  # Juvnile Summary Table 
  output$rstsumm_table <- DT::renderDataTable({
    rst_tmp <- juv_df()
    DT::datatable(rst_tmp, options = list(orderClasses = TRUE, 
                                          autoWidth = TRUE, 
                                          dom = 'tpl'),
                  filter = 'top') 
  })
  
  
  #//////////////////////////////////////////////////////////////////////////////
  # Hydro Tab
  #//////////////////////////////////////////////////////////////////////////////
  # Hydro project selection
  output$hydro_menu <- renderUI({
    selectInput('hydro_locs', label = 'Hydrosystem Project:', choices = c('Lower Granite Dam' = 'LWG',
                                                                          'Bonneville Dam' = 'BON'),
                selectize = FALSE, size =5, multiple = TRUE)
  })
  # Hydro year selection
  output$hydro_year_menu <- renderUI({
    choose_year <- 2006:year(Sys.Date())
    sliderInput("hydro_year", label = 'Year:', min = min(choose_year), max = max(choose_year), step = 1, value = max(choose_year), sep='')
  })

   # Gather river conditions from DART
    flow_df <- eventReactive(input$hydro_reset, {
      queryRiverData(site = input$hydro_locs, 
                             year = input$hydro_year,
                             start_day = '01/01',
                             end_day = '12/31') %>%
          mutate_all(as.character) %>% 
          mutate(Dam = case_when(
            Site == 'LWG' ~ 'Lower Granite',
            Site == 'BON' ~ 'Bonneville')) %>%
          select(Dam, Site, Date, everything())
    })
    
   # Gather window count data
    window_df <- eventReactive(input$hydro_reset, {
           queryWindowCnts(dam = 'LWG', #input$hydro_locs,
                      spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
                      spawn_yr = input$hydro_year,
                      start_day = '01/01',
                      end_day = '12/31') %>%
              mutate(Chinook = Chinook + Jack_Chinook,
                     Coho = Coho + Jack_Coho) %>%
              select(Year, Date, Chinook, Coho, Steelhead, Wild_Steelhead, Lamprey)
    })
    
   # Hydro Species selection
    output$hydro_species_menu <- renderUI({
      choose_spp <- sort(names(window_df()[-c(1,2)]))# c('Chinook', 'Coho', 'Steelhead', 'Wild_Steelhead', 'Lamprey')
      selectInput("hydro_spp", label = 'Species:', choices = choose_spp, selectize = FALSE, size = 3, multiple = FALSE)
    })
  # Hydro Metric selection
   output$hydro_metric_menu <- renderUI({
     metrics <- sort(names(flow_df()[-(1:3)]))  #c('Inflow', 'Temperature', 'TDG') #
     selectInput('river_metric', 'River Metric (y-axis)', choices = metrics, selected = 'Inflow', selectize = TRUE)
   })
   
   # Plot window counts
   output$window_plot <- renderPlot({
     window_df() %>%
       ggplot(aes(x = as.Date(Date),
                  y = as.numeric(!!as.symbol(input$hydro_spp)))) +
       geom_bar(stat = 'identity') +
       scale_colour_viridis_d() +
       scale_x_date(date_labels = format('%b-%d')) +
       theme_bw() +
       theme(legend.position = 'none') +
       labs(x = 'Date',
            y = 'Daily Count',#input$hydro_spp,
            subtitle = paste0('Daily window count of ', input$hydro_spp, ' at ',input$hydro_locs,'  Dam during ', isolate(input$hydro_year),'.'),
            title = 'Window Counts'
       )
   }) 

   # Plot river conditions
   output$river_plot <- renderPlot({
     flow_df() %>%
       ggplot(aes(x = as.Date(Date), y = as.numeric(!!as.symbol(input$river_metric)))) +
       geom_line(colour = '#536872', size = 1) +
       scale_x_date(date_labels = format('%b-%d')) +
       theme_bw() +
       theme(legend.position = 'none') +
       labs(x = 'Date',
            y = input$river_metric,
            subtitle = paste0('Daily ', input$river_metric, ' at ',input$hydro_locs,'  Dam during ', isolate(input$obs_year),'.'),
            title = 'Mainstem River Conditions'
            )
   })

# Fish Management Menu (No Server Code) ----
   
# Reporting Menu (No Server Code) ----
   
#  Raw Data Menu ----
   
  # Dataset selection
  output$raw_dataset_menu <- renderUI({
     
    dataset <- datasets %>%
      select(DatastoreId, DatastoreName) %>%
      distinct(DatastoreId, .keep_all = TRUE) %>%
      arrange(DatastoreName)
     
    datasets_ls <- as.list(dataset[,1])
    names(datasets_ls) <- dataset[,2]
    selectInput("datasets", h3("Data Type:"), choices = datasets_ls, selectize = TRUE)
  }) 

  # get the full dataset view
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
   
  # Dataset EXPORT
  output$raw_export <- downloadHandler(
    filename = function() {
      paste0(input$datasets,"_raw_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(raw_dat(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

 # Dataset Data Table
  output$raw_table <- DT::renderDataTable({
    
    tmp_df <- raw_dat()
    DT::datatable(tmp_df, options = list(orderClasses = TRUE), filter = 'top')
    # , 
    #               caption = paste0('Dataset generated from the Nez Perce Tribes centralized data base managment system
    #                                and should be cited accordingly. Query parameters were set as; ', raw_qry_params()))
  })
  
  

}) # Close Server
