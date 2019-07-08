# Kus Server ----
server <- function(input, output, session) {
  # Hide & show Tabs based on login status ----
  observe({
    if(is.null(login_status)) {
      hideElement(selector = "ul li:eq(9)", anim= TRUE) # Number is the "list item" (tags$li and menuItems) to remove(x-1)), # change as tabs are included in sidebar
    } else {
      if(status_code(login_status) != 200) {
        hideElement(selector = "ul li:eq(9)", anim= TRUE) # change as tabs are included in sidebar
      } else {
        showElement(selector = "ul li:eq(9)", anim= TRUE) # change as tabs are included in sidebar
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
    hideElement(selector = "ul li:eq(9)", anim= TRUE) # change as tabs are included in sidebar
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
        showElement(selector = "ul li:eq(9)") # change as tabs are included in sidebar
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
    
  })

  # Weir Collections Summaries Tab ----
  
  # In-Stream Array Abundance Summaries Tab ----

  # Juvenile Monitoring Summaries Tab ----
  
  observeEvent(input$juv_submit, {
    
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
        layout(separators = ',')
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
      filter(!DatastoreId %in% c(81:84, 88:91)) %>%
      add_row(DatastoreId= c(999,998), DatastoreName = c('SGS Summary', 'RST Summary')) %>%
      arrange(DatastoreName)
    
    datasets_ls <- as.list(dataset[,1])
  
    names(datasets_ls) <- dataset[,2]
    selectInput("datasets", h3("Choose Data Type:"), choices = datasets_ls, selectize = TRUE)
  }) 
  
    # get the full dataset view
  raw_dat <- eventReactive(input$raw_submit,{
    
    shinyjs::show(id='datasets_spinner')
    
    if(input$datasets != 999 & input$datasets != 998) {  
      getDatasetView(datastoreID = input$datasets, projectID = NULL, waterbodyID = NULL, locationID = NULL, cdms_host = cdms_host)
    } else {
      if(input$datasets != 998) {
      summariseSGS()
    } else {
        summariseRST()
      }
    }
    
    shinyjs::hide(id='datasets_spinner')
    
  }) 
  
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
  })
  
  # Custom Queries ----
  
    # Dataset selection
  if(html_code == 200){
    datastore_df <- getDatastores(cdms_host = cdms_host) %>%
      select(Id, Name) %>%
      filter(!Id %in% c(81:84, 88:91))
  }
  
  output$q_datasets <- renderUI({
    selectInput(inputId = 'q_datasets', label = NULL, choices = c('- Choose Dataset -', sort(unique(datastore_df$Name))),
                selectize = FALSE, selected = '- Choose Dataset -', multiple = FALSE)
  })

  observeEvent(input$btn_q_datasets, {
    # match selected Dataset to datastore_df$Id
    ds_Id <- datastore_df$Id[match(input$q_datasets, datastore_df$Name)]
    
    shinyjs::show(id='query_spinner')
    
    # Load the Big Dataset and populate the first selectInput(species)
    if(input$q_datasets == '- Choose Dataset -') {
      NULL
    } else {
      if(input$q_datasets %in% c("SGS Redd Data", "SGS Carcass Data")) {
        
        # Remove any existing query inputs (5x because can't figure out the right selector command)
        removeUI(selector = '#q_inputs')
        removeUI(selector = '#q_inputs')
        removeUI(selector = '#q_inputs')
        removeUI(selector = '#q_inputs')
        removeUI(selector = '#q_inputs')
        
        # Get Dataset View. (Load Big Data)
        KQ_df0 <- getDatasetView(datastoreID = ds_Id, cdms_host = cdms_host) %>%
          mutate(Year = year(SurveyDate),
                 SpeciesRun = paste(Run, Species))
        
        # Species selection
        output$q_species <- renderUI({
          selectInput(inputId= 'q_species', label= 'Choose Species', choices= sort(unique(KQ_df0$SpeciesRun)),
                      selectize= TRUE, selected = NULL, multiple = TRUE) 
        })
        
      } else {
        
        # Remove any existing query inputs (5x because can't figure out the right selector command)
        removeUI(selector = '#q_inputs')
        removeUI(selector = '#q_inputs')
        removeUI(selector = '#q_inputs')
        removeUI(selector = '#q_inputs')
        removeUI(selector = '#q_inputs')
        
        # Get Dataset View. (Load Big Data)
        KQ_df0 <- getDatasetView(datastoreID = ds_Id, cdms_host = cdms_host) %>%
          mutate(Year = year(BroodYear),
                 SpeciesRun = paste(Run, Species))
        
        # Species selection
        output$q_species <- renderUI({
          selectInput(inputId= 'q_species', label= 'Choose Species', choices= sort(unique(KQ_df0$SpeciesRun)),
                      selectize= TRUE, selected = NULL, multiple = TRUE) 
        })
      } 
    }
    
    shinyjs::hide(id='query_spinner')
    
  })
  
  
    # 1. POP_NAME selection ----
  observeEvent(input$q_species, {
    if(is.null(input$q_species)) {
      NULL
    } else {
      KQ_df1 <<- KQ_df0 %>%
        filter(SpeciesRun %in% input$q_species)
      
      output$q_pop_name <- renderUI({
        div(id = 'q_inputs',  # this div() allows us to remove the input later
            selectInput(inputId= 'q_pop_name', label= 'Choose Population:', choices= sort(unique(KQ_df1$POP_NAME)),
                        selectize= TRUE, selected = NULL, multiple = TRUE)
        )
      })
    }
  })
  
    # 2. Stream selection ----
  observeEvent(input$q_pop_name, {
    if(is.null(input$q_pop_name)) {
      NULL
    } else {
      KQ_df2 <<- KQ_df1 %>%
        filter(POP_NAME %in% input$q_pop_name)
      
      output$q_stream <- renderUI({
        div(id = 'q_inputs',  # this div() allows us to remove the input later
            selectInput(inputId= 'q_stream', label= 'Choose Stream:', choices= sort(unique(KQ_df2$StreamName)),
                        selectize= TRUE, selected = NULL, multiple = TRUE)
        )
      })
    }
  })
  
    # 3. LocationLabel selection ----
  observeEvent(input$q_stream, {
    if(is.null(input$q_stream)) {
      NULL
    } else {
      KQ_df3 <<- KQ_df2 %>%
        filter(StreamName %in% input$q_stream)
      
      output$q_locationlabel <- renderUI({
        div(id = 'q_inputs',  # this div() allows us to remove the input later
            selectInput(inputId= 'q_locationlabel', label= 'Choose Location:', choices= sort(unique(KQ_df3$LocationLabel)),
                        selectize= TRUE, selected = NULL, multiple = TRUE)
        )
      })
    }
  })
  
    # 4. Year selection and Submit Query button ----
  observeEvent(input$q_locationlabel, {
    if(is.null(input$q_locationlabel)) {
      NULL
    } else {
      KQ_df4 <<- KQ_df3 %>%
        filter(LocationLabel %in% input$q_locationlabel)
      
      output$q_year <- renderUI({
        div(id = 'q_inputs',  # this div() allows us to remove the input later
            sliderInput(inputId= 'q_year', label= 'Choose Year:', min = min(KQ_df4$Year), max = max(KQ_df4$Year), 
                        value=  c(min(KQ_df4$Year), max(KQ_df4$Year)), sep= '')
        )
      })
      
    # 5. 'Submit Query' Button
      output$btn_q_summary <- renderUI({
        div(id = 'q_inputs',  # this div() allows us to remove the input later
            actionButton(inputId = 'btn_q_summary', label = 'Submit Query', icon = icon('bomb'))
        )
      })
    }
  })
  
    # Query Data Table ----
      # Produce FINAL Query Dataframe for table/export
    query_df <- eventReactive(input$btn_q_summary, {
      KQ_df4 %>%
        filter(Year %in% input$q_year)
    })    
  
    # Query-dataframe EXPORT
    output$query_export <- downloadHandler(
      filename = function() {
        paste0("query_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(query_df(), file, row.names = FALSE)
      },
      contentType = "text/csv"
    )

  output$query_table <- DT::renderDataTable({
    tmp_qdf <- query_df()
    DT::datatable(tmp_qdf, options = list(orderClasses = TRUE), filter = 'top')
  })
  
  # Reports
  
} # close Server
