#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# library(shiny)
# library(tidyverse)
# library(httr)
# library(plotly)
# library(leaflet)
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

# Outside Server - Static metadata tables
# Need to set tribal specific variables
cdms_host <- 'https://cdms.nptfisheries.org'
#cdms_host <- 'localhost'
# username <- 'api_user'
# api_key <- "153054453130053281239582410943958241094537726538860542262540750542640375910349488180619663"


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

# get redd data
# redd_df <- getDatasetView(datastoreID = 78, cdms_host = cdms_host)
# tmp_df <- getDatasetView(datastoreID = 80, cdms_host = cdms_host)
load('./data/redd_df.rda')

redd_df <- mutate_at(redd_df, .funs = as.numeric, .vars = c('NewRedds', 'Latitude', 'Longitude')) %>%
mutate(SppRun = paste0(Species, " - ", Run))

# Dummy Carcass Data for SGS Summary
load('./data/carc_df.Rda')

redd_locs <- redd_df %>%
  filter(!is.na(Latitude),
         !is.na(Longitude))

# get map data
load('./data/map_data.Rdata')

# get river flow data
river_df <- bind_rows(queryRiverData(site = 'LWG',
                                     year = 2018, #year(Sys.Date()),
                                     start_day = '01/01',
                                     end_day = '12/31') %>% #format(Sys.Date(), '%m/%d')) %>%
                        mutate_all(as.character),
                      queryRiverData(site = 'BON',
                                     year = 2018, #year(Sys.Date()),
                                     start_day = '01/01',
                                     end_day = '12/31') %>% #format(Sys.Date(), '%m/%d')) %>%
                        mutate_all(as.character)) %>%
  mutate(Dam = ifelse(Site == 'LWG', 'Lower Granite', 'Bonneville'),
         Date = as.Date(Date),
         Inflow = as.numeric(Inflow)) %>%

  select(Dam, Site, Date, everything())

# get window count
win_df <- bind_rows(queryWindowCnts(dam = 'LWG', spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
                                    spawn_yr = 2018, #year(Sys.Date()),
                                    start_day = '01/01',
                                    end_day = '12/31') %>% #format(Sys.Date(), '%m/%d')) %>%
                      mutate(Site = 'LWG'),
                    queryWindowCnts(dam = 'BON', spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
                                    spawn_yr = 2018, #year(Sys.Date()),
                                    start_day = '01/01',
                                    end_day = '12/31') %>% #format(Sys.Date(), '%m/%d')) %>%
                      mutate(Site = 'BON')) %>%
  mutate(Chinook = Chinook + Jack_Chinook,
         Coho = Coho + Jack_Coho,
         Dam = ifelse(Site == 'LWG', 'Lower Granite', 'Bonneville'),
         Date = as.Date(Date)) %>%
  select(Site, Dam, Date, Chinook, Coho, Steelhead, Wild_Steelhead, Lamprey)


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
                  # datasets <- getDatastores(cdms_host = cdms_host) %>%  # gather CDMS Data
                  #   rename(DatastoreId = Id, DatastoreName = Name)
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
  
  output$redd_map <- renderLeaflet({
    
    pal <- colorFactor(palette = rainbow(3),
                       redd_df$SppRun)
    
    groups <- as.character(unique(redd_df$SppRun))
    
    map <- leaflet(redd_df[redd_df$SurveyYear==year(Sys.Date())-2,]) %>%
      setView(lat = 45.65,
              lng = -115.85,
              zoom = 7) %>%
      setMaxBounds(lng1 = -122,
                   lat1 = 42,
                   lng2 = -110,
                   lat2 = 49) %>%
      addProviderTiles(providers$Esri.WorldTopoMap,
                       options = providerTileOptions(minZoom = 6))
    
    for(g in groups){
      d = redd_df[redd_df$SurveyYear==year(Sys.Date())-2 & redd_df$SppRun == g,]
      map = map %>% addCircleMarkers(data = d, lat = ~Latitude, lng = ~Longitude, label = ~WPTName,
                                     group = g, color = ~pal(SppRun),
                                     #clusterOptions = markerClusterOptions(),
                                     fillOpacity = .25)
    }

    map %>% 
      addLayersControl(
        overlayGroups = groups,
        options = layersControlOptions(collapsed = TRUE)) %>%
      addLegend(pal = pal, values = ~SppRun, title = '', position = 'bottomleft')

  })

  # 1. Trend redd counts

  output$home_redd <- renderPlotly({
    
    p <- redd_df %>%
      distinct(ActivityId, .keep_all = TRUE) %>%
      group_by(ESU, MPG, POP, SppRun, SurveyYear) %>%
      summarise(TotalRedds = sum(NewRedds, na.rm = TRUE)) %>%
      ggplot(aes(x = SurveyYear, y = TotalRedds)) +
      geom_line(aes(colour = POP), size = 1) +
      geom_point(aes(colour = POP), size = 2) +
      scale_colour_viridis_d() +
      facet_wrap(~SppRun, ncol = 1, scale = 'free') +
      theme_grey() +
      labs(x = 'Survey Year',
           y = 'Total Redds',
           colour = 'Population',
           title = '')
    
    ggplotly(p, height = 700) %>% layout(margin = list(b = 50, l = 90))
    
  })
  
  # 2. River Flow and Spill

  output$home_river <- renderPlotly({
    p <- river_df %>%
      ggplot(aes(x = Date)) +
      geom_line(aes(y = Inflow), colour = 'darkblue', size = 1) +
      scale_x_date(date_labels = format('%b-%d')) +
      facet_wrap(~Dam, nrow =1) +
      theme_grey() +
      labs(x = 'Date',
           y = 'Inflow (kcfs)',
           title = '')

    ggplotly(p) %>% layout(margin = list(b = 50, l = 90))
  })

  # 3. Window Counts

    output$home_BONwin <- renderPlotly({
    p <- win_df %>%
      filter(Dam == 'Bonneville') %>%
      gather(species, count, Chinook:Lamprey) %>%
      ggplot(aes(x = Date, y = count)) +
      geom_bar(aes(fill = species), stat = 'identity') +
      scale_fill_viridis_d() +
      scale_x_date(date_labels = format('%b-%d')) +
      facet_grid(species~Dam, scales = 'free_y') +
      theme_grey() +
      theme(legend.position = 'none') +
      labs(x = 'Date',
           y = 'Daily Window Count',
           title = ''
      )

    ggplotly(p) %>% layout(margin = list(b = 50, l = 90))
  })

  output$home_LGRwin <- renderPlotly({
    p <- win_df %>%
      filter(Dam == 'Lower Granite') %>%
      gather(species, count, Chinook:Lamprey) %>%
      ggplot(aes(x = Date, y = count)) +
      geom_bar(aes(fill = species), stat = 'identity') +
      scale_fill_viridis_d() +
      scale_x_date(date_labels = format('%b-%d')) +
      facet_grid(species~Dam, scales = 'free_y') +
      #facet_wrap(~Dam, scales = 'free_x') +
      theme_grey() +
      theme(legend.position = 'none') +
      labs(x = 'Date',
           y = 'Daily Window Count',
           title = ''
      )

    ggplotly(p) %>% layout(margin = list(b = 50, l = 90))
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
  
  #-----------------------------------------------------------------
  #  Summarized SGS Data
  #-----------------------------------------------------------------
  # stream selection
  output$streams_menu <- renderUI({
    summ_reddstreams <- c('Choose Streams' = '', as.character(sort(unique(redd_df$StreamName))))   
    selectInput('summ_streams', label = NULL, choices = summ_reddstreams, selectize = TRUE, multiple = TRUE)
  })
  
  # SGS Summary Tab -----------------------------------------------
  summary_tmp <- eventReactive(input$summ_reset,{

    #redd_df <- getDatasetView(datastoreID = 78, projectID = NULL, waterbodyID = NULL, locationID = NULL, cdms_host = cdms_host)
    #carc_df <- getDatasetView(datastoreID = 79, projectID = NULL, waterbodyID = NULL, locationID = NULL, cdms_host = cdms_host)

    tmp1 <- redd_df %>%
      distinct(ActivityId, .keep_all = TRUE) %>%
      group_by(StreamName, ESU, MPG, POP, SppRun, SurveyYear) %>%  
      summarise(TotalRedds = sum(NewRedds, na.rm = TRUE)) %>%
      ungroup() %>%
      select(StreamName, SurveyYear, TotalRedds)

    TEMP <- carc_df %>%
      filter(`Target Species` == 'S_CHN') %>%
      select(`Location`, `ActivityDate`, `Sex`, `Count`, `Percent Spawned`, `Spawned Out`, `Adipose Fin Clipped`,
             `Snout Collected`, `CWT Code`) %>%
      separate(ActivityDate, into = 'ActivityDate', sep = ' 12:00:00 AM') %>%
      separate(Location, int = c('StreamName', 'Transect'), sep = ": ") %>%
      mutate(ActivityDate = mdy(ActivityDate),
             SurveyYear = year(ActivityDate),
             Origin = ifelse(`Adipose Fin Clipped` == 'Yes', "Hatchery",
                             ifelse(`Adipose Fin Clipped` == "No", "Natural",    # Not Necessarily TRUE!!!!!!!
                                    ifelse(`Snout Collected` == 'Yes', "Hatchery",
                                           ifelse(!is.na(`CWT Code`), "Hatchery", "Natural"))))
      )

    # %F
    PF_tmp <- TEMP %>%
      filter(Sex %in% c('Male', 'Female')) %>%
      group_by(StreamName, SurveyYear, Sex) %>%
      summarise(Count = sum(Count, na.rm = TRUE)) %>%
      spread(key = Sex, value = Count, fill = 0) %>%
      mutate(`%Females` = round(100*(`Female`/(`Female` + `Male`)), 2))

    # pHOS
    phos_tmp <- TEMP %>%
      filter(Origin %in% c('Natural', 'Hatchery')) %>%
      group_by(StreamName, SurveyYear, Origin) %>%
      summarise(Count = sum(Count, na.rm = TRUE)) %>%
      spread(key = Origin, value = Count, fill = 0) %>%
      mutate(pHOS = round(100*(`Hatchery`/(`Hatchery` + `Natural`)), 2))

    # Prespawn Mortality
    psm_tmp <- TEMP %>%
      filter(Sex == "Female") %>%
      mutate(PrespawnMort = ifelse(`Spawned Out` == "No", "PrespawnMort", 
                                   ifelse(`Percent Spawned` %in% c('0%', '25%'), "PrespawnMort", "spawned"))) %>%
      group_by(StreamName, SurveyYear, PrespawnMort ) %>%
      summarise(Count = sum(Count, na.rm = TRUE)) %>%
      spread(key = `PrespawnMort`, value = Count, fill = 0) %>%
      select(-spawned)

    # Total Carcasses
    all_carc <- TEMP %>%
      group_by(StreamName, SurveyYear) %>%
      summarise(`Carcass Total` = sum(Count, na.rm = TRUE))

    # Join Redd/Carcass summaries
    FINAL <- left_join(tmp1, PF_tmp, by = c('StreamName', 'SurveyYear')) %>%
      left_join(phos_tmp, by = c('StreamName', 'SurveyYear')) %>%
      left_join(psm_tmp, by = c('StreamName', 'SurveyYear')) %>%
      left_join(all_carc, by = c('StreamName', 'SurveyYear')) %>%
      filter(StreamName == input$summ_streams) %>%
      rename('Stream Name' = StreamName, 'Year' = SurveyYear,
             'Hatchery Origin' = Hatchery, 'Natural Origin' = Natural, '% Hatchery Spawners' = pHOS,
             'Prespawn Mortalities' = PrespawnMort)
  })

  # SGS: Redd/Carcass Summary Table
  output$summ_table <- DT::renderDataTable({
    tmp_summ <- summary_tmp()
    DT::datatable(tmp_summ, options = list(orderClasses = TRUE), filter = 'top')
  })

  
}) # Close Server
