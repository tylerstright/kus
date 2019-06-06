# Kus Server ----
server <- function(input, output, session) {
  # Hide & show Tabs based on login status ----
  observe({
    if(is.null(login_status)) {
      hideElement(selector = "ul li:eq(8)", anim= TRUE) # Number is the "list item" (tags$li and menuItems) to remove(x-1))
    } else {
      if(status_code(login_status) != 200) {
        hideElement(selector = "ul li:eq(8)", anim= TRUE)
      } else {
        showElement(selector = "ul li:eq(8)", anim= TRUE)
        }
    }
  })
  
  # Create Login / Logout Functionality ----
    # Login
  output$login_logout <- renderUI({
    if(is.null(login_status)) {
      actionLink('login_link', '[Sign-In]', icon = icon('sign-in-alt'), style = 'color: white;')
    } else {
      if(status_code(login_status) == 200) {
        actionLink('logout_link', label = paste(user_info()$Fullname, ' [Sign Out]'),
                   icon = icon('sign-out-alt'), style = 'color: white;')
      } else {
        actionLink('login_link', 'Sign In', icon = icon('sign-in-alt'), style = 'color: white;')
      }
    }
  })  
    # Logout
  observeEvent(input$logout_link, {
    login_status <<- NULL
    hideElement(selector = "ul li:eq(8)", anim= TRUE)
    output$login_logout <- renderUI({actionLink('login_link', '[Sign In]', icon = icon('sign-in-alt'), style = 'color: white;')}) 
  })
  
  # Login Modal Process ----
  
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
                 footer = "Please contact Clark Watry (clarkw@nezperce.org) to request login credentials."
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
        showElement(selector = "ul li:eq(8)")
        output$login_logout <- renderUI({
          actionLink('logout_link', label = paste(user_info()$Fullname, ' [Sign Out]'),
                     icon = icon('sign-out-alt'), style = 'color: white;')
        })
      }
    }
  })
  

  
  # HOME Tab ----
  
  # SGS Miles Surveyed    ////  NEED: Transect Lengths.
  output$sgs_totalmiles <- renderValueBox({
    
    total_mi <- rtd_df %>%
      distinct(ActivityId, .keep_all = TRUE) %>%
      summarise(Total = round(sum(`Transect Length`, na.rm=TRUE), 0)) %>%
      pull()
    
    valueBox(
      value = prettyNum(total_mi, big.mark = ","), 
      subtitle ='Total Stream Kilometers Surveyed', 
      icon = icon('binoculars', lib = 'font-awesome'), 
      color ='aqua')
  })
  output$sgs_groundmiles <- renderValueBox({
    
    ground_mi <- rtd_df %>%
      distinct(ActivityId, .keep_all = TRUE) %>%
      filter(SurveyMethod == 'Ground') %>%
      summarise(Total = round(sum(`Transect Length`, na.rm=TRUE), 0)) %>%
      pull()
    
    valueBox(
      value = prettyNum(ground_mi, big.mark = ","), 
      subtitle ='Stream Kilometers Surveyed on Foot', 
      icon = icon('walking', lib = 'font-awesome'), 
      color ='aqua')
  })
  output$sgs_airmiles <- renderValueBox({
    air_mi <- rtd_df %>%
      distinct(ActivityId, .keep_all = TRUE) %>%
      filter(SurveyMethod == 'Helicopter') %>%
      summarise(Total = round(sum(`Transect Length`, na.rm=TRUE), 0)) %>%
      pull()
    
    valueBox(
      value = prettyNum(air_mi, big.mark = ","),
      subtitle ='Stream Kilometers Surveyed by Helicopter', 
      icon = icon('plane', lib = 'font-awesome'), # helicopter 
      color ='aqua')
  })
  
  # Home MAP
  output$home_map <- renderLeaflet({

    l <- leaflet(options = leafletOptions(minZoom = 7, doubleClickZoom = FALSE)) %>%
      setView(lat = 45.8,
              lng = -116.1,
              zoom = 7) %>%
      setMaxBounds(lng1 = -119,
                   lat1 = 42,
                   lng2 = -114,
                   lat2 = 49) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addPolygons(data = icc, fill = FALSE,
                  color = 'black', weight = 2, opacity = 1, group = '1855 Reservation') %>%
      addPolygons(data = npt1863, fill = 'red',
                  color = 'black', weight = 2, opacity = .25, group = 'Nez Perce Reservation') %>%
      addScaleBar(position = 'topright')
  })
  
  # Project Count
    # Get Projects
      projects <- getProjects(cdms_host = "https://cdms.nptfisheries.org")
    # Create datatable for Modal
  output$project_dt <- renderTable({
    project_list <- projects %>%
      pull(Name)
  })
    # Project Count valueBox
  output$project_count <- renderValueBox({
    project_count <- projects %>%
      summarise(Total = n()) %>%
      pull()
    
    valueBox(#title = "Number of NPT Projects", 
            value = project_count,
            color = 'aqua',
            # subtitle = HTML("<b>Number of NPT Projects</b> <button id=\"button\" type=\"button\" class=\"btn btn-default action-button\">Show modal</button>")            
            subtitle = HTML("<b>Number of NPT Projects</b><a id=\"project_count_btn\" href=\"#\" class=\"action-button\">
     <i class=\"fa fa-question-circle\"></i>
                            
                            </a>")
    )
  })
    # Show Modal on click
  observeEvent(input$project_count_btn, {
    toggleModal(session, "project_count_modal", "open")
  })
  
  # NPT Datastores Count
  
    # Create datatable for Modal
  output$dataset_dt <- renderTable({
    project_list <- datasets %>%
      filter(!DatastoreId %in% c(81:84, 88:91)) %>%
      pull(DatastoreName)
  })
    # infoBox
  output$dataset_count <- renderInfoBox({
    
    dataset_count <- datasets %>%
      filter(!DatastoreId %in% c(81:84, 88:91)) %>%
      summarise(Total = n()) %>%
      pull()
    
    infoBox(title = HTML("<b>Number of NPT Datastores</b><a id=\"dataset_count_btn\" href=\"#\" class=\"action-button\">
     <i class=\"fa fa-question-circle\"></i>
                         
                         </a>"), 
            value = dataset_count,
            color = 'aqua'
    )
  })
    # Show Modal on click
  observeEvent(input$dataset_count_btn, {
    toggleModal(session, "dataset_count_modal", "open")
  })
  
  # Adult Summaries Tab ----
  
  # Total Redds per Year
  output$p_redds <- renderPlotly({
    yr_df <- dsv_78 %>%
      mutate(Year = year(SurveyDate),
             SpeciesRun = paste(Run, SpeciesName)) %>%
      distinct(ActivityId, .keep_all = TRUE) %>%
      filter(SpeciesRun == input$species) %>% 
      group_by(POP_NAME, SpeciesRun, Year) %>%
      summarise(`TotalRedds` = sum(NewRedds, na.rm=TRUE)) %>%
      arrange(Year)
    
    yr_plotly <- plot_ly(data = yr_df, 
                         x = ~Year, 
                         y = ~TotalRedds,
                         type = 'scatter',
                         mode = 'lines+markers',
                         color = ~POP_NAME,
                         colors = viridis_pal(option="D")(length(unique(yr_df$POP_NAME)))
                         ) #%>%
                           #layout(legend = list(orientation = 'h'))
  })
  
  # Total Carcasses per Year
  output$p_carcass <- renderPlotly({
    yc_df <- dsv_79 %>%
      mutate(Year = year(SurveyDate),
             SpeciesRun = paste(Run, SpeciesName)) %>%
      #filter(SpeciesRun == input$species) %>% 
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
    ) #%>%
    #layout(legend = list(orientation = 'h'))
  })
  
  # Juvenile Summaries Tab ----
  
  # Natural Juvenile Abundance
  output$j_abundance <- renderPlotly({
    ja_df <- dsv_85 %>%
      mutate(Year = MigratoryYear,
             SpeciesRun = paste(Run, SpeciesName)) %>%
      filter(!is.na(Abundance),
             Lifestage == 'Total',
             SpeciesRun == input$species) %>%
      arrange(Year)
      
    ja_plotly <- plot_ly(data = ja_df, 
                         x = ~Year, 
                         y = ~Abundance,
                         type = 'scatter',
                         mode = 'lines+markers',
                         color = ~POP_NAME,
                         colors = viridis_pal(option="D")(length(unique(ja_df$POP_NAME)))
    ) #%>%
    #layout(legend = list(orientation = 'h'))
  })
  
  # Natural Juvenile Abundance
  output$j_survival <- renderPlotly({
    js_df <- dsv_86 %>%
      mutate(Year = MigratoryYear,
             SpeciesRun = paste(Run, SpeciesName)) %>%
      filter(!is.na(Survival),
             SpeciesRun == input$species) %>%
      arrange(Year)
    
    ja_plotly <- plot_ly(data = js_df, 
                         x = ~Year, 
                         y = ~Survival,
                         type = 'scatter',
                         mode = 'lines+markers',
                         color = ~POP_NAME,
                         colors = viridis_pal(option="D")(length(unique(js_df$POP_NAME))),
                         linetype = ~Lifestage,
                         markers = ~Origin
    ) #%>%
    #layout(legend = list(orientation = 'h'))
  })
  
  # Juvenile Arrival Timing
  # queryJuvPITarrival & arrival_timing.R (Tech Teams)
  
  # Productivity Summaries Tab ----
  
  # Raw Data Access Tab ----

    # Dataset & Data Summary selection
  output$raw_dataset_menu <- renderUI({
    
    dataset <- datasets %>%
      select(DatastoreId, DatastoreName) %>%
      distinct(DatastoreId, .keep_all = TRUE) %>%
      filter(!DatastoreId %in% c(81:84, 88:91)) %>%
      add_row(DatastoreId=999, DatastoreName = 'SGS Summary') %>%
      arrange(DatastoreName)
    
    datasets_ls <- as.list(dataset[,1])
  
    names(datasets_ls) <- dataset[,2]
    selectInput("datasets", h3("Choose Data Type:"), choices = datasets_ls, selectize = TRUE)
  }) 
  
    # get the full dataset view
  raw_dat <- eventReactive(input$raw_submit,{
    if(input$datasets != 999) {  # we want this to NOT equal the summary [datasetId]s which don't exist in CDMS
      getDatasetView(datastoreID = input$datasets, projectID = NULL, waterbodyID = NULL, locationID = NULL, cdms_host = cdms_host)
    } else {
      summariseSGS()
      }
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
    # , 
    #               caption = paste0('Dataset generated from the Nez Perce Tribes centralized data base managment system
    #                                and should be cited accordingly. Query parameters were set as; ', raw_qry_params()))
  })

  
} # close Server
