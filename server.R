# Kus Server ----
server <- function(input, output, session) {
  # Hide & show Tabs based on login status ----
  observe({
    if(is.null(login_status)) {
      hideElement(selector = "ul li:eq(11)", anim= TRUE) # Number is the "list item" (tags$li and menuItems) to remove(x-1)), # change as tabs are included in sidebar
    } else {
      if(status_code(login_status) != 200) {
        hideElement(selector = "ul li:eq(11)", anim= TRUE) # change as tabs are included in sidebar
      } else {
        showElement(selector = "ul li:eq(11)", anim= TRUE) # change as tabs are included in sidebar
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
    hideElement(selector = "ul li:eq(11)", anim= TRUE) # change as tabs are included in sidebar
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
        showElement(selector = "ul li:eq(11)") # change as tabs are included in sidebar
        output$login_logout <- renderUI({
          actionLink('logout_link', label = paste(user_info()$Fullname, ' [Sign Out]'),
                    icon = icon('sign-out-alt'), style = 'color: white;')
        })
      }
    }
  })
  

  
  # HOME Tab ----
  
  # SGS Miles Surveyed
  # output$sgs_totalmiles <- renderValueBox({
  #   
  #   total_mi <- rtd_df %>%
  #     distinct(ActivityId, .keep_all = TRUE) %>%
  #     summarise(Total = round(sum(`Transect Length`, na.rm=TRUE), 0)) %>%
  #     pull()
  #   
  #   valueBox(
  #     value = prettyNum(total_mi, big.mark = ","), 
  #     subtitle ='Total Stream Kilometers Surveyed', 
  #     icon = icon('binoculars', lib = 'font-awesome'), 
  #     color ='aqua')
  # })
  # output$sgs_groundmiles <- renderValueBox({
  #   
  #   ground_mi <- rtd_df %>%
  #     distinct(ActivityId, .keep_all = TRUE) %>%
  #     filter(SurveyMethod == 'Ground') %>%
  #     summarise(Total = round(sum(`Transect Length`, na.rm=TRUE), 0)) %>%
  #     pull()
  #   
  #   valueBox(
  #     value = prettyNum(ground_mi, big.mark = ","), 
  #     subtitle ='Stream Kilometers Surveyed on Foot', 
  #     icon = icon('walking', lib = 'font-awesome'), 
  #     color ='light-blue')
  # })
  # output$sgs_airmiles <- renderValueBox({
  #   air_mi <- rtd_df %>%
  #     distinct(ActivityId, .keep_all = TRUE) %>%
  #     filter(SurveyMethod == 'Helicopter') %>%
  #     summarise(Total = round(sum(`Transect Length`, na.rm=TRUE), 0)) %>%
  #     pull()
  #   
  #   valueBox(
  #     value = prettyNum(air_mi, big.mark = ","),
  #     subtitle ='Stream Kilometers Surveyed by Helicopter', 
  #     icon = icon('helicopter', lib = 'font-awesome'), # helicopter or plane
  #     color ='teal')
  # })
  
  # Project Count
    # Get Projects
      # projects <- getProjects(cdms_host = "https://cdms.nptfisheries.org")
    # Create datatable for Project Count Modal
  # output$project_dt <- renderTable({
  #   project_list <- projects %>%
  #     pull(Name)
  # })
    # Project Count valueBox
  # output$project_count <- renderInfoBox({
  #   project_count <- projects %>%
  #     summarise(Total = n()) %>%
  #     pull()
  #   
  #   infoBox(value = project_count,
  #           color = 'aqua',
  #           title = HTML("<b>Number of NPT Projects</b><a id=\"project_count_btn\" href=\"#\" class=\"action-button\">
  #    <i class=\"fa fa-question-circle\"></i>
  #                           
  #                           </a>")
  #   )
  # })
    # Show Project Count Modal on click
  # observeEvent(input$project_count_btn, {
  #   toggleModal(session, "project_count_modal", "open")
  # })
  # 
  # NPT Datastores Count

    # Create datatable for Datastore Count Modal
  # output$dataset_dt <- renderTable({
  #   datasets %>%
  #     filter(!DatastoreId %in% c(81:84, 88:91)) %>%
  #     pull(DatastoreName)
  # })
    # Datastore Count infoBox
  # output$dataset_count <- renderInfoBox({
  #   
  #   dataset_count <- datasets %>%
  #     filter(!DatastoreId %in% c(81:84, 88:91)) %>%
  #     summarise(Total = n()) %>%
  #     pull()
  #   
  #   infoBox(title = HTML("<b>Number of NPT Datastores</b><a id=\"dataset_count_btn\" href=\"#\" class=\"action-button\">
  #    <i class=\"fa fa-question-circle\"></i>
  #                        
  #                        </a>"), 
  #           value = dataset_count,
  #           color = 'aqua'
  #   )
  # })

    # Show Datastore Count Modal on click
  # observeEvent(input$dataset_count_btn, {
  #   toggleModal(session, "dataset_count_modal", "open")
  # })
  
  # Spawning Ground Surveys Summaries Tab ----

  observeEvent(input$sgs_submit, {
   
  disable(id = 'sgs_submit')
  shinyjs::show(id='sgs_spinner')
  
  # Get Redd data
  tmp_redd_df <- getDatasetView(datastoreID = 78, cdms_host = cdms_host) %>%
    mutate(Year = year(SurveyDate),
           SpeciesRun = paste(Run, Species))
  
    # Get Carcass data
    tmp_carcass_df <- getDatasetView(datastoreID = 79, cdms_host = cdms_host) %>%
      mutate(Year = year(SurveyDate),
             SpeciesRun = paste(Run, Species))  
    
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
                           mode = 'lines+markers'#,
                           # color = ~POP_NAME,
                           # colors = viridis_pal(option="D")(length(unique(yr_df$POP_NAME)))
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
    
    shinyjs::hide(id='sgs_spinner')
    enable(id = 'sgs_submit')
    
  })

  # Weir Collections Summaries Tab ----
  
  # In-Stream Array Abundance Summaries Tab ----

  # Juvenile Monitoring Summaries Tab ----
  
  observeEvent(input$juv_submit, {
    
    disable(id = 'juv_submit')
    shinyjs::show(id='juv_spinner')
    
    # Get Abundance data
    tmp_abundance_df <- getDatasetView(datastoreID = 85, cdms_host = cdms_host) %>%
      mutate(Year = MigratoryYear,
             SpeciesRun = paste(Run, Species))
    
    # # Get Survival data
    tmp_survival_df <- getDatasetView(datastoreID = 86, cdms_host = cdms_host) %>%
      mutate(Year = MigratoryYear,
             SpeciesRun = paste(Run, Species))
    
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

      ja_plotly <- plot_ly(data = js_df,
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
  
    shinyjs::hide(id='juv_spinner')
    enable(id= 'juv_submit')
    
  })
  
  # Restricted Data Access Tab ----

  # Gather static session data from CDMS
  if(html_code == 200){
    datasets <- getDatastores(cdms_host = cdms_host) %>%
      rename(DatastoreId = Id, DatastoreName = Name)
  }
  
  # CDMS Datasets 
  output$raw_dataset_menu <- renderUI({
    
    dataset <- datasets %>%
      select(DatastoreId, DatastoreName) %>%
      distinct(DatastoreId, .keep_all = TRUE) %>%
      filter(!DatastoreId %in% c(81:84, 87:91)) %>%  # removed DM Issues (87) as well
      arrange(DatastoreName)
    
    datasets_ls <- as.list(dataset[,1])
  
    names(datasets_ls) <- dataset[,2]
    selectInput("datasets", label = '1. Choose Dataset:', choices = datasets_ls, selected = NULL, selectize = TRUE, width = '100%')
  }) 
  

  observeEvent(input$raw_submit,{
    # hide/show inputs
    disable(id = 'raw_submit')
    disable(id = 'datasets')
    shinyjs::show(id='datasets_spinner')
    shinyjs::hide(id='q_species')  
    shinyjs::hide(id='q_pop_name')
    shinyjs::hide(id='q_stream')
    shinyjs::hide(id='q_locationlabel')
    shinyjs::hide(id='q_year')
    shinyjs::hide(id='dataset_field_select')
    shinyjs::hide(id='dataset_field_submit')
    shinyjs::hide(id='raw_export')
    shinyjs::hide(id='raw_table')

    # Get the full dataset view
      raw_dat <- getDatasetView(datastoreID = input$datasets, projectID = NULL, waterbodyID = NULL, locationID = NULL, cdms_host = cdms_host) %>%
        mutate(SpeciesRun = paste(Run, Species))

    # Loop! Waiting for data load.  (So elegant!) 
    i <- 1
    while(i < 1000) {

      delay(ms= 1000, i <- i+1)

      if(exists(x= 'raw_dat') == FALSE) { # if raw_dat DOESN'T exist -> loop continues.
        NULL
      } else { # until raw_dat DOES exist
        
        if(input$datasets %in% c(78, 79)) { # =c("SGS Redd Data", "SGS Carcass Data")
          # Prepare Adult Data (i.e. Survey Date)
          raw_dat1 <<- raw_dat %>%
            mutate(SurveyDate = as_date(SurveyDate),
                   Year = year(SurveyDate))
        } else {
          if(input$datasets %in% c(85, 86)) { # = c('NPT RST Abundance Estimates', 'NPT Juvenile Survival Estimates')
          # Prepare Juvenile Data (i.e. Brood Year)
          raw_dat1 <<- raw_dat %>%
            mutate(Year = MigratoryYear)
          } else {
            # Prepare Age Data (i.e. Collection Date)
            raw_dat1 <<- raw_dat %>%
              mutate(CollectionDate = as_date(CollectionDate),
                     Year = year(CollectionDate))
          }
        }

        # Species selection
        output$q_species <- renderUI({
          selectInput(inputId= 'q_species', label= '2. Choose Species', choices= sort(unique(raw_dat1$SpeciesRun)),
                      selectize= TRUE, selected = NULL, multiple = TRUE) 
        })
        
        shinyjs::hide(id='datasets_spinner')
        shinyjs::show(id='q_species')
        enable(id = 'raw_submit')
        enable(id = 'datasets')

        break # (stop the loop)
      }
    }
    
  })
  
    # POP_NAME Selection
  observeEvent(input$q_species, {
    if(is.null(input$q_species)) {
      NULL
    } else {
      raw_dat2 <<- raw_dat1 %>%
        filter(SpeciesRun %in% input$q_species)
      
      output$q_pop_name <- renderUI({
            selectInput(inputId= 'q_pop_name', label= '3. Choose Population:', choices= sort(unique(raw_dat2$POP_NAME)),
                        selectize= TRUE, selected = NULL, multiple = TRUE)
      })
      
      shinyjs::show(id='q_pop_name')
      
    }
  })
  
    # Stream selection ----
  observeEvent(input$q_pop_name, {
    if(is.null(input$q_pop_name)) {
      NULL
    } else {
        raw_dat3 <<- raw_dat2 %>%
          filter(POP_NAME %in% input$q_pop_name)
        
        output$q_stream <- renderUI({
              selectInput(inputId= 'q_stream', label= '4. Choose Stream:', choices= sort(unique(raw_dat3$StreamName)),
                          selectize= TRUE, selected = NULL, multiple = TRUE)
        })
        
      shinyjs::show(id='q_stream')
      
      }
  })
  
    # LocationLabel selection ----
  observeEvent(input$q_stream, {
    if(is.null(input$q_stream)) {
      NULL
    } else {
      raw_dat4 <<- raw_dat3 %>%
        filter(StreamName %in% input$q_stream)
      
      output$q_locationlabel <- renderUI({
            selectInput(inputId= 'q_locationlabel', label= '5. Choose Location:', choices= sort(unique(raw_dat4$LocationLabel)),
                        selectize= TRUE, selected = NULL, multiple = TRUE)
      })
      
      shinyjs::show(id='q_locationlabel')
      
    }
  })
  
    # Year selection, Field Selection, and Submit Query button ----
  observeEvent(input$q_locationlabel, {
    if(is.null(input$q_locationlabel)) {
      NULL
    } else {
      raw_dat5 <<- raw_dat4 %>%
        filter(LocationLabel %in% input$q_locationlabel)
      
      output$q_year <- renderUI({
        tagList(
            sliderInput(inputId= 'q_year', label= '6. Choose Year:', min = min(raw_dat5$Year), max = max(raw_dat5$Year), 
                        value=  c(min(raw_dat5$Year), max(raw_dat5$Year)), sep= '', step = 1),
            helpText(HTML('<em>*Year is Spawn Year for adult datasets and Migratory Year for juveniles datasets.</em>'), style = 'text-align:center;')
        )
      })
      
      # Field selectors.
        raw_dat_fields <- names(raw_dat5) # create field list
            
      output$dataset_fields <- renderUI({
        selectInput(inputId = 'dataset_field_select', label = '7. Choose Fields in Desired Order:', choices = raw_dat_fields, selectize = TRUE, multiple = TRUE)
      })

      output$datasetfield_submit <- renderUI({
        actionButton(inputId = 'dataset_field_submit', label = 'Populate Table', width = '100%', icon = icon('table'))
      })
      
      shinyjs::show(id='q_year')
      shinyjs::show(id='dataset_field_select')
      shinyjs::show(id='dataset_field_submit')
      
    }
  })

    # Apply Select statement to data and produce datatable on submit
    observeEvent(input$dataset_field_submit, {
      
      shinyjs::show(id='raw_export')
      shinyjs::show(id='raw_table')
      
      # Prep data
      if(is.null(input$dataset_field_select)) {
        raw_dat_final <<- raw_dat5 %>%
          filter(Year %in% input$q_year)
      } else {
      raw_dat_final <<- raw_dat5 %>%
        filter(Year %in% input$q_year) %>%
        select(input$dataset_field_select)
    }
      # Create table
      output$raw_table <- DT::renderDataTable({
        DT::datatable(raw_dat_final, options = list(orderClasses = TRUE), filter = 'top')
      })

    })
    
    
    # Dataset EXPORT
  output$raw_export <- downloadHandler(
    filename = function() {
      paste0(input$datasets,"_raw_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(raw_dat_final, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # Custom Queries ----
  
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
      
      disable(id='custom_query_menu')
      disable(id='custom_submit')
      hide(id='custom_field_select')
      hide(id='custom_field_submit')
      hide(id='custom_export')
      hide(id='custom_table')
      
      
      if(input$custom_query_menu == 'RST Summary') {
        custom_dat <<- summariseRST()
      } else {
        custom_dat <<- summariseSGS() #'SGS Summary'
      }
    
    # Populate Field selector
      custom_dat_fields <- names(custom_dat) # create field list
      
      output$custom_fields <- renderUI({
        tagList(
          helpText(HTML('<em>Select desired fields in preferred order.</em>'), style='text-align:center;'),
          selectInput(inputId = 'custom_field_select', label = NULL, choices = custom_dat_fields, selectize = TRUE, multiple = TRUE)
        )
      })
      
      output$customfield_submit <- renderUI({
        tagList(
          helpText(HTML('<em>Click to apply field selections.</em>'), style='text-align:center;'),
          actionButton(inputId = 'custom_field_submit', label = 'Populate Table', width = '100%', icon = icon('table'))
        )
      })
      

    enable(id='custom_query_menu')
    enable(id='custom_submit')
    show(id='custom_field_select')
    show(id='custom_field_submit')
    
    }
  })
  
  # Apply Field Selection and create Custom Table
  observeEvent(input$custom_field_submit, {
    
    show(id='custom_export')
    show(id='custom_table')
    
    # Prep data
    if(!is.null(input$custom_field_select)) {
      custom_dat <<- custom_dat %>%
        select(input$custom_field_select)
    } 
    
    # Create table
    output$custom_table <- DT::renderDataTable({
      DT::datatable(custom_dat, options = list(orderClasses = TRUE), filter = 'top')
    })
    
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
