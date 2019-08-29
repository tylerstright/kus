# Kus Server ----
server <- function(input, output, session) {
  # Hide & show Tabs based on login status ----
  observe({
    if(is.null(login_status)) {
      # hideElement(selector = "ul li:eq(12)", anim= TRUE) # Number is the "list item" (tags$li and menuItems) to remove(x-1)), # change as tabs are included in sidebar
    } else {
      if(status_code(login_status) != 200) {
        # hideElement(selector = "ul li:eq(12)", anim= TRUE) # change as tabs are included in sidebar
      } else {
        # showElement(selector = "ul li:eq(12)", anim= TRUE) # change as tabs are included in sidebar
        }
    }
  })
  
  # Create Login / Logout Functionality ----
    # Login
  output$login_logout <- renderUI({
    if(is.null(login_status)) {
      actionLink('login_link', '[Sign-In]', icon = icon('sign-in-alt'),
                 style = 'color: white;')
    } else {
      if(status_code(login_status) == 200) {
        actionLink('logout_link', label = paste0(user_info()$Fullname, ' [Sign Out]'),
                   icon = icon('sign-out-alt'),
                   style = 'color: white;')
      } 
    }
  })  
    # Logout
  observeEvent(input$logout_link, {
    login_status <<- NULL
    # hideElement(selector = "ul li:eq(12)", anim= TRUE) # change as tabs are included in sidebar
    output$login_logout <- renderUI({actionLink('login_link', '[Sign In]', icon = icon('sign-in-alt'), style = 'color: white;')}) 
  })
  
  # Login Modal Process ----
  makeReactiveBinding("login_status")
    # User information
  user_info <- reactive({
    httr::content(login_status, "parsed", encoding = "UTF-8")[[3]]
  })
    # Modal
  observeEvent(input$login_link,
               showModal(modalDialog(
                 textInput('username','Username', width = "100%"), 
                 passwordInput('password', 'Password', width = "100%"), 
                 tags$head(tags$script(HTML(jscode))),
                 actionButton('login', 'Login'),
                 size = "m",
                 easyClose = TRUE,
                 title = "DFRM Fisheries Data Access",
                 footer = HTML("<em> Restricted data access is intended for DFRM Staff only. </em>")
               ))
  )
  observeEvent(input$login, {
    
    if(input$username == '' | input$password == '')
    {
      showModal(modalDialog(
        textInput('username','Username', width = "100%"),
        passwordInput('password', 'Password', width = "100%"),
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
          textInput('username','Username', width = "100%"),
          passwordInput('password', 'Password', width = "100%"),
          tags$head(tags$script(HTML(jscode))),
          actionButton('login', 'Login'),
          size = "m",
          easyClose = TRUE,
          title = "Invalid Username or Password",
          footer = "I'm sorry you are having trouble. Try re-checking the post-it note on your computer screen. :)"
        ))
      } else {
        removeModal()
        # showElement(selector = "ul li:eq(12)") # change as tabs are included in sidebar
        output$login_logout <- renderUI({
          actionLink('logout_link', label = paste(user_info()$Fullname, ' [Sign Out]'),
                    icon = icon('sign-out-alt'), style = 'color: white;')
        })
      }
    }
  })
  

  
  # HOME Tab ----

  # Spawning Ground Surveys Summaries Tab ----
  
  # Load SGS (Redd & Carcass) data
  observeEvent(input$sgs_dataload, {
    disable(id = 'sgs_dataload')
    shinyjs::show(id='sgs_spinner')

   # Get Redd data
   tmp_redd_df <<- getDatasetView(datastoreID = 78, cdms_host = cdms_host) %>%
     mutate(Year = year(SurveyDate),
            SpeciesRun = paste(Run, Species))

   # Get Carcass data
   tmp_carcass_df <<- getDatasetView(datastoreID = 79, cdms_host = cdms_host) %>%
     mutate(Year = year(SurveyDate),
            SpeciesRun = paste(Run, Species))

   sgs_pop_list_full <<- tmp_redd_df %>%
     select(SpeciesRun, POP_NAME) %>%
     bind_rows(tmp_carcass_df %>% select(SpeciesRun, POP_NAME)) %>%
     group_by(SpeciesRun, POP_NAME) %>%
     filter(POP_NAME != 'NA') %>%
     dplyr::distinct(POP_NAME) %>%
     arrange(POP_NAME)
   
   sgs_species_list <- as.list(unique(sgs_pop_list_full$SpeciesRun))

   output$sgs_species <- renderUI({
   selectInput(inputId= 'sgs_species', label= 'Choose Species:', choices= sgs_species_list, selectize= FALSE,
                      selected = 'Fall Chinook Salmon', multiple = FALSE)
   })

  hide(id= 'sgs_spinner')
  })
  
  observeEvent(input$sgs_species, {
    
  sgs_population_list <- sgs_pop_list_full %>%
    filter(SpeciesRun == input$sgs_species) %>%
    pull(POP_NAME)

  output$sgs_pop_name <- renderUI({
    selectInput(inputId= 'sgs_pop_name', label= 'Choose Population:', choices= sgs_population_list, selectize= FALSE, multiple = FALSE,
                selected= NULL)
  })

  })

  observeEvent(input$sgs_pop_name, {
   
      # Total Redds per Year
    output$p_redds <- renderPlotly({
      yr_df <- tmp_redd_df %>%
        distinct(ActivityId, .keep_all = TRUE) %>%
        filter(SpeciesRun == isolate(input$sgs_species),
               POP_NAME %in% isolate(input$sgs_pop_name)) %>%
        group_by(POP_NAME, SpeciesRun, Year) %>%
        summarise(`TotalRedds` = sum(NewRedds, na.rm=TRUE)) %>%
        arrange(Year)
  
      yr_plotly <- plot_ly(data = yr_df,
                           x = ~Year,
                           y = ~TotalRedds,
                           type = 'scatter',
                           mode = 'lines+markers'
                           ) %>%
        layout(yaxis = list(title= 'Total Redds'))
    })

    # Total Carcasses per Year
    output$p_carcass <- renderPlotly({
      yc_df <- tmp_carcass_df %>%
        filter(SpeciesRun == isolate(input$sgs_species),
               POP_NAME %in% isolate(input$sgs_pop_name)) %>%
        group_by(POP_NAME, SpeciesRun, Year) %>%
        summarise(`TotalCarcass` = sum(Count, na.rm=TRUE)) %>%
        arrange(Year)
  
      yc_plotly <- plot_ly(data = yc_df,
                           x = ~Year,
                           y = ~TotalCarcass,
                           type = 'scatter',
                           mode = 'lines+markers',
                           color = ~POP_NAME,
                           colors = viridis_pal(option="D")(length(unique(yc_df$POP_NAME)))
      ) %>%
        layout(yaxis = list(title= 'Total Carcasses'))
    })
    
  })

  # Weir Collections Summaries Tab ----
  
  # In-Stream Array Abundance Summaries Tab ----

  # Juvenile Monitoring Summaries Tab ----
  
  # Load Juvenile Summary data (Abundance & Survival) data
  observeEvent(input$juv_dataload, {
    disable(id = 'juv_dataload')
    shinyjs::show(id='juv_spinner')
    
    # Get Abundance data
    tmp_abundance_df <<- getDatasetView(datastoreID = 85, cdms_host = cdms_host) %>%
      mutate(Year = MigratoryYear,
             SpeciesRun = paste(Run, Species))

    # # Get Survival data
    tmp_survival_df <<- getDatasetView(datastoreID = 86, cdms_host = cdms_host) %>%
      mutate(Year = MigratoryYear,
             SpeciesRun = paste(Run, Species))
    
    tmp_equivalents_df <<- left_join(tmp_abundance_df, tmp_survival_df, by = c('POP_NAME','Lifestage', 'Year', 'SpeciesRun', 'Origin')) %>%
      group_by(POP_NAME, Origin, SpeciesRun, Year, Lifestage) %>%
      mutate(Equivalents = round(Abundance*Survival, digits = 0))
    
    juv_pop_list_full <<- tmp_abundance_df %>%
      select(SpeciesRun, POP_NAME) %>%
      bind_rows(tmp_survival_df %>% select(SpeciesRun, POP_NAME)) %>%
      group_by(SpeciesRun, POP_NAME) %>%
      filter(POP_NAME != 'NA') %>%
      dplyr::distinct(POP_NAME) %>%
      arrange(POP_NAME)
    
    juv_species_list <- as.list(unique(juv_pop_list_full$SpeciesRun))
    
    output$juv_species <- renderUI({
      selectInput(inputId= 'juv_species', label= 'Choose Species:', choices= juv_species_list, selectize= FALSE, 
                  selected = 'Fall Chinook Salmon', multiple = FALSE)
    })
    
    hide(id= 'juv_spinner')
  })
  
  observeEvent(input$juv_species, {
    
    juv_population_list <- juv_pop_list_full %>%
      filter(SpeciesRun == input$juv_species) %>%
      pull(POP_NAME)
    
    output$juv_pop_name <- renderUI({
      selectInput(inputId= 'juv_pop_name', label= 'Choose Population:', choices= juv_population_list, selectize= FALSE, 
                  selected = 'Snake River Lower Mainstem', multiple = FALSE)
    })
    
  })
  
  observeEvent(input$juv_pop_name, {
    
    # Natural Juvenile Abundance
    output$j_abundance <- renderPlotly({
      ja_df <- tmp_abundance_df %>%
        filter(!is.na(Abundance),
               !Lifestage %in% c('YOY','Total'),
               Origin == 'Natural',
               SpeciesRun == isolate(input$juv_species),
               POP_NAME %in% isolate(input$juv_pop_name)) %>%
        arrange(Year)
      
      ja_plotly <- plot_ly(data = ja_df,
                           x = ~Year,
                           y = ~Abundance,
                           type = 'scatter',
                           mode = 'lines+markers', 
                           linetype = ~Lifestage, 
                           color = ~POP_NAME,
                           colors = viridis_pal(option="D")(length(unique(ja_df$POP_NAME)))
      ) %>%
        layout(yaxis= list(hoverformat= ',.'))
    })
    
    # Natural Juvenile Survival
    output$j_survival <- renderPlotly({
      js_df <- tmp_survival_df %>%
        filter(!is.na(Survival),
               Origin == 'Natural',
               SpeciesRun == isolate(input$juv_species),
               POP_NAME %in% isolate(input$juv_pop_name)) %>%
        arrange(Year)

      js_plotly <- plot_ly(data = js_df,
                           x = ~Year,
                           y = ~Survival,
                           type = 'scatter',
                           mode = 'lines+markers',
                           color = ~POP_NAME,
                           colors = viridis_pal(option="D")(length(unique(js_df$POP_NAME))),
                           linetype = ~Lifestage
                           # markers = ~Origin
      ) %>%
        layout(yaxis= list(tickformat = "%"))
    })
    
    # Natural Juvenile Equivalents
    output$j_equivalents <- renderPlotly({
      je_df <- tmp_equivalents_df %>%
        filter(!is.na(Equivalents),
               Origin == 'Natural',
               SpeciesRun == isolate(input$juv_species),
               POP_NAME %in% isolate(input$juv_pop_name)) %>%
        arrange(Year) %>%
        ungroup()
      
      je_plotly <- plot_ly(data = je_df,
                           x = ~Year,
                           y = ~Equivalents,
                           type = 'scatter',
                           mode = 'lines+markers',
                           color = ~POP_NAME,
                           colors = viridis_pal(option="D")(length(unique(je_df$POP_NAME))),
                           linetype = ~Lifestage
                           # markers = ~Origin
      ) %>%
        layout(yaxis= list(hoverformat= ',.'))
    })
  
  })
  
  # Age Samples Tab ----
  observeEvent(input$age_summary_btn, {
    shinyjs::disable(id='age_summary_btn')
    shinyjs::show(id='age_spinner')
  
  age_graphs <- summariseAGE() 
  
  output$age_total <- renderPlotly({
    age_graphs[[1]]
  }) 
  
  output$age_ocean <- renderPlotly({
    age_graphs[[2]]
  })  
  
  output$age_stream <- renderPlotly({
    age_graphs[[3]]
  })
  
    shinyjs::hide(id='age_spinner')
  })
  
  # Restricted Data Access Tab ----
  # Create ***REACTIVE VALUES*** for dynamic data and Inputs ----
  RV <- reactiveValues(query_data = NULL)
  # Clear Field Values Button ----
  observeEvent(input$clear_fields, {
    if(exists('raw_dat') != TRUE) {
      NULL
    } else {
    RV$query_data <<- raw_dat   # will this reset all?
    
      updateSelectInput(session, inputId= 'q_species', label= 'Choose Species:', choices= sort(unique(RV$query_data$SpeciesRun)),
                        selected = NULL) 
      updateSelectInput(session, inputId= 'q_pop_name', label= 'Choose Population:', choices= sort(unique(RV$query_data$POP_NAME)),
                        selected = NULL) 
      updateSelectInput(session, inputId= 'q_stream', label= 'Choose Stream:', choices= sort(unique(RV$query_data$StreamName)),
                        selected = NULL) 
      updateSelectInput(session, inputId= 'q_transect', label= 'Choose Transect:', choices= sort(unique(RV$query_data$LocationLabel)),
                        selected = NULL) 
      updateSelectInput(session, inputId= 'q_fields', label= 'Choose Fields in Desired Order:', choices= names(RV$query_data), selected = NULL) 
      # q-year?
      updateSelectInput(session, inputId= 'test', label= 'CLEAR!!!!!! WORKED!', choices= sort(unique(RV$query_data$LocationLabel)),
                        selected = NULL) 
    }
  })
  
  #TESTING OUTPUT
  output$testing <- renderText({
    paste('InputYear = ', input$q_year, 'min is ;', min(input$q_year))
  })
  
  # Gather/Create dataset list from CDMS ----
  if(html_code == 200){
    datasets <- getDatastores(cdms_host = cdms_host) %>%
      rename(DatastoreId = Id, DatastoreName = Name)
  }
  
  output$raw_dataset_menu <- renderUI({
    
    dataset <- datasets %>%
      select(DatastoreId, DatastoreName) %>%
      distinct(DatastoreId, .keep_all = TRUE) %>%
      filter(!DatastoreId %in% c(81:84, 87:91)) %>% 
      arrange(DatastoreName)
    
    datasets_ls <- as.list(dataset[,1])
  
    names(datasets_ls) <- dataset[,2]
    selectInput("datasets", label = 'Choose Dataset:', choices = datasets_ls, selected = NULL, selectize = TRUE, width = '100%')
  }) 
  
  # Load Data ----
  observeEvent(input$raw_submit,{
    # hide/show inputs
    disable(id = 'raw_submit')
    disable(id = 'datasets')
    shinyjs::show(id='datasets_spinner')

    if(input$datasets %in% c(78, 79)) { # =c("SGS Redd Data", "SGS Carcass Data")
      raw_dat <<- getDatasetView(datastoreID = input$datasets, projectID = NULL, waterbodyID = NULL, locationID = NULL, cdms_host = cdms_host) 
      # Prepare Adult Data (i.e. Survey Date)
      raw_dat <<- raw_dat %>%
        mutate(SpeciesRun = paste(Run, Species),
               SurveyDate = as_date(SurveyDate),
               Year = year(SurveyDate))
    } else {
      if(input$datasets %in% c(85, 86)) { # = c('NPT RST Abundance Estimates', 'NPT Juvenile Survival Estimates')
        raw_dat <<- getDatasetView(datastoreID = input$datasets, projectID = NULL, waterbodyID = NULL, locationID = NULL, cdms_host = cdms_host)
      # Prepare Juvenile Data (i.e. Migratory Year)
      raw_dat <<- raw_dat %>%
        mutate(SpeciesRun = paste(Run, Species),
               Year = MigratoryYear)
      } else {
        raw_dat <<- getDatasetView(datastoreID = input$datasets, projectID = NULL, waterbodyID = NULL, locationID = NULL, cdms_host = cdms_host)
        # Prepare Age Data (i.e. Collection Date)
        raw_dat <<- raw_dat %>%
          mutate(SpeciesRun = paste(Run, Species),
                 CollectionDate = as_date(CollectionDate),
                 Year = year(CollectionDate))
      }
      
      raw_dat <<- raw_dat %>% select(-contains('Id'))
      
    }
  
      RV$query_data <<- raw_dat  # Populate our dynamic dataframe.
      
      # Update our Inputs
      updateSelectInput(session, inputId= 'q_species', label= 'Choose Species:', choices= sort(unique(RV$query_data$SpeciesRun)),
                        selected = NULL) 
      updateSelectInput(session, inputId= 'q_pop_name', label= 'Choose Population:', choices= sort(unique(RV$query_data$POP_NAME)),
                        selected = NULL) 
      updateSelectInput(session, inputId= 'q_stream', label= 'Choose Stream:', choices= sort(unique(RV$query_data$StreamName)),
                        selected = NULL) 
      updateSelectInput(session, inputId= 'q_transect', label= 'Choose Transect:', choices= sort(unique(RV$query_data$LocationLabel)),
                        selected = NULL) 
      updateSelectInput(session, inputId= 'q_fields', label= 'Choose Fields in Desired Order:', choices= names(RV$query_data), selected = NULL) 
      
      updateSliderInput(session, inputId = 'q_year', label = '*Choose Years:', min = min(RV$query_data$Year), max = max(RV$query_data$Year), 
                        value = c(min(RV$query_data$Year), max(RV$query_data$Year)), step = 1)
      
      updateSelectInput(session, inputId= 'test', label= 'DATA LOAD WORKED!', choices= sort(unique(RV$query_data$LocationLabel)),
                        selected = NULL) 

        shinyjs::hide(id='datasets_spinner')
        enable(id = 'raw_submit')
        enable(id = 'datasets')
  })
  
  # Input Reactivity ----
  observeEvent(input$q_species, {
                   
                   # save existing input values
                   selected_pop <- input$q_pop_name
                   selected_stream <- input$q_stream
                   # selected_transect <- input$q_transect
                   
                   RV$query_data <<- raw_dat  # re-Populate our dynamic dataframe.
                   
                   # Apply filters
                   if(!is.null(input$q_species)) {
                     RV$query_data <<- RV$query_data %>% filter(SpeciesRun %in% input$q_species)
                   }
                   
                   if(!is.null(input$q_pop_name)) {
                     RV$query_data <<- RV$query_data %>% filter(POP_NAME %in% input$q_pop_name)
                   }
                   
                   if(!is.null(input$q_stream)) {
                     RV$query_data <<- RV$query_data %>% filter(StreamName %in% input$q_stream)
                   }
                   
                   if(!is.null(input$q_transect)) {
                     RV$query_data <<- RV$query_data %>% filter(LocationLabel %in% input$q_transect)
                   }
                   
                   updateSelectInput(session, inputId= 'q_pop_name', label= 'Choose Population:', choices= sort(unique(RV$query_data$POP_NAME)),
                                     selected = selected_pop)
                   updateSelectInput(session, inputId= 'q_stream', label= 'Choose Stream:', choices= sort(unique(RV$query_data$StreamName)),
                                     selected = selected_stream)
                   # updateSelectInput(session, inputId= 'q_transect', label= 'Choose Transect:', choices= sort(unique(RV$query_data$LocationLabel)),
                   #                   selected = selected_transect)
                 })
  # POP
  observeEvent(input$q_pop_name, {
                   
                   # save existing input values
                   selected_species <- input$q_species
                   selected_stream <- input$q_stream
                   # selected_transect <- input$q_transect
                   
                   RV$query_data <<- raw_dat  # re-Populate our dynamic dataframe.
                   
                   # Apply filters
                   if(!is.null(input$q_species)) {
                     RV$query_data <<- RV$query_data %>% filter(SpeciesRun %in% input$q_species)
                   }
                   
                   if(!is.null(input$q_pop_name)) {
                     RV$query_data <<- RV$query_data %>% filter(POP_NAME %in% input$q_pop_name)
                   }
                   
                   if(!is.null(input$q_stream)) {
                     RV$query_data <<- RV$query_data %>% filter(StreamName %in% input$q_stream)
                   }
                   
                   if(!is.null(input$q_transect)) {
                     RV$query_data <<- RV$query_data %>% filter(LocationLabel %in% input$q_transect)
                   }
                   
                   updateSelectInput(session, inputId= 'q_species', label= 'Choose Species:', choices= sort(unique(RV$query_data$SpeciesRun)),
                                     selected = selected_species)
                   updateSelectInput(session, inputId= 'q_stream', label= 'Choose Stream:', choices= sort(unique(RV$query_data$StreamName)),
                                     selected = selected_stream)
                   # updateSelectInput(session, inputId= 'q_transect', label= 'Choose Transect:', choices= sort(unique(RV$query_data$LocationLabel)),
                   #                   selected = selected_transect)
                 })
  # Stream
  observeEvent(input$q_stream, {
                   
                   # save existing input values
                   selected_species <- input$q_species
                   selected_pop <- input$q_pop_name
                   # selected_transect <- input$q_transect
                   
                   RV$query_data <<- raw_dat  # re-Populate our dynamic dataframe.
                   
                   # Apply filters
                   if(!is.null(input$q_species)) {
                     RV$query_data <<- RV$query_data %>% filter(SpeciesRun %in% input$q_species)
                   }
                   
                   if(!is.null(input$q_pop_name)) {
                     RV$query_data <<- RV$query_data %>% filter(POP_NAME %in% input$q_pop_name)
                   }
                   
                   if(!is.null(input$q_stream)) {
                     RV$query_data <<- RV$query_data %>% filter(StreamName %in% input$q_stream)
                   }
                   
                   if(!is.null(input$q_transect)) {
                     RV$query_data <<- RV$query_data %>% filter(LocationLabel %in% input$q_transect)
                   }
                   
                   updateSelectInput(session, inputId= 'q_species', label= 'Choose Species:', choices= sort(unique(RV$query_data$SpeciesRun)),
                                     selected = selected_species)
                   updateSelectInput(session, inputId= 'q_pop_name', label= 'Choose Population:', choices= sort(unique(RV$query_data$POP_NAME)),
                                     selected = selected_pop)
                   # updateSelectInput(session, inputId= 'q_transect', label= 'Choose Transect:', choices= sort(unique(RV$query_data$LocationLabel)),
                   #                   selected = selected_transect)
                 })
  # Transect
  # observeEvent(input$q_transect, {
  #                  
  #                  # save existing input values
  #                  selected_species <- input$q_species
  #                  selected_pop <- input$q_pop_name
  #                  selected_stream <- input$q_stream
  #                  
  #                  RV$query_data <<- raw_dat  # re-Populate our dynamic dataframe.
  #                  
  #                  # Apply filters
  #                  if(!is.null(input$q_species)) {
  #                    RV$query_data <<- RV$query_data %>% filter(SpeciesRun %in% input$q_species)
  #                  }
  #                  
  #                  if(!is.null(input$q_pop_name)) {
  #                    RV$query_data <<- RV$query_data %>% filter(POP_NAME %in% input$q_pop_name)
  #                  }
  #                  
  #                  if(!is.null(input$q_stream)) {
  #                    RV$query_data <<- RV$query_data %>% filter(StreamName %in% input$q_stream)
  #                  }
  #                  
  #                  if(!is.null(input$q_transect)) {
  #                    RV$query_data <<- RV$query_data %>% filter(LocationLabel %in% input$q_transect)
  #                  }
  #                  
  #                  updateSelectInput(session, inputId= 'q_species', label= 'Choose Species:', choices= sort(unique(RV$query_data$SpeciesRun)),
  #                                    selected = selected_species)
  #                  updateSelectInput(session, inputId= 'q_pop_name', label= 'Choose Population:', choices= sort(unique(RV$query_data$POP_NAME)),
  #                                    selected = selected_pop)
  #                  updateSelectInput(session, inputId= 'q_stream', label= 'Choose Stream:', choices= sort(unique(RV$query_data$StreamName)),
  #                                    selected = selected_stream)
  #                })
  
  
  
  # THIS IS THE GOOD ONE!!! ----
  # observeEvent(c(input$q_species,
  #                input$q_pop_name,
  #                input$q_stream,
  #                input$q_transect), {
  #     
  #     # save existing input values
  #     selected_species <- input$q_species
  #     selected_pop <- input$q_pop_name
  #     selected_stream <- input$q_stream
  #     selected_transect <- input$q_transect
  #     
  #     RV$query_data <<- raw_dat  # re-Populate our dynamic dataframe.
  #     
  #     # Apply filters
  #     if(!is.null(input$q_species)) {
  #       RV$query_data <<- RV$query_data %>% filter(SpeciesRun %in% input$q_species)
  #     }
  #     
  #     if(!is.null(input$q_pop_name)) {
  #       RV$query_data <<- RV$query_data %>% filter(POP_NAME %in% input$q_pop_name)
  #     }
  #     
  #     if(!is.null(input$q_stream)) {
  #       RV$query_data <<- RV$query_data %>% filter(StreamName %in% input$q_stream)
  #     }
  #     
  #     if(!is.null(input$q_transect)) {
  #       RV$query_data <<- RV$query_data %>% filter(LocationLabel %in% input$q_transect)
  #     }
  # 
  #     updateSelectInput(session, inputId= 'q_species', label= 'Choose Species:', choices= sort(unique(RV$query_data$SpeciesRun)),
  #                       selected = selected_species)
  #     updateSelectInput(session, inputId= 'q_pop_name', label= 'Choose Population:', choices= sort(unique(RV$query_data$POP_NAME)),
  #                       selected = selected_pop)
  #     updateSelectInput(session, inputId= 'q_stream', label= 'Choose Stream:', choices= sort(unique(RV$query_data$StreamName)),
  #                       selected = selected_stream)
  #     updateSelectInput(session, inputId= 'q_transect', label= 'Choose Transect:', choices= sort(unique(RV$query_data$LocationLabel)),
  #                       selected = selected_transect)
  #   })
  

  # Create CDMS Dataset table (reactive/self-updating) ----
  output$raw_table <- DT::renderDataTable({
    
    shiny::validate(
      need(RV$query_data, message = '    Table will populate after data load.')
    )

    if(is.null(input$q_fields)) {
      table_data <- RV$query_data %>%
        filter(Year %in% input$q_year)
    } else {
      table_data <- RV$query_data %>% 
        filter(Year %in% input$q_year) %>%
        select(input$q_fields)
      }

    DT::datatable(table_data, options = list(orderClasses = TRUE), filter = 'top')
  })
  
    # CDMS Dataset EXPORT ----
  output$raw_export <- downloadHandler(
    filename = function() {
      paste0(input$datasets,"_raw_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(raw_dat_final, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # Custom Queries (CUSTOM!) --------------------------------------------------
  
  # Dynamic Description for selected Query
  output$query_description <- renderText({
    # match Query with Description and paste value
    q_description <- custom_query_df$query_descriptions[match(input$custom_query_menu, custom_query_df$query_names)]
    paste0("Description: ", q_description)
  })

  # Submit Query Request
  observeEvent(input$custom_submit, {
    
    if(input$custom_query_menu == '-Select Custom Query-') {
      NULL
    } else {
      
      show(id='query_spinner')
      disable(id='custom_query_menu')
      disable(id='custom_submit')

      if(input$custom_query_menu == 'RST Summary') {
        RV$cq_data <<- summariseRST()
      } else {
        RV$cq_data <<- summariseSGS() #'SGS Summary'
      }
    
      updateSelectInput(session, inputId= 'cq_fields', label= 'Choose Fields in Desired Order:', choices= names(RV$cq_data), selected = NULL) 

    enable(id='custom_query_menu')
    enable(id='custom_submit')
    hide(id='query_spinner')
    
    }
  })
  
  # Apply Field Selection and create Custom Table ----
  output$custom_table <- DT::renderDataTable({
    
    if(is.null(input$cq_fields)) {
      table_data <- RV$cq_data 
    } else {
      table_data <- RV$cq_data %>% 
        select(input$cq_fields)
    }
    
    DT::datatable(table_data, options = list(orderClasses = TRUE), filter = 'top')  
  })
  
  # Dataset EXPORT
  output$custom_export <- downloadHandler(
    filename = function() {
      paste0(input$custom_query_menu,"_custom_query_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(custom_dat, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # Reports ----
  
} # close Server
