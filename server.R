# Kus Server ----
server <- function(input, output, session) {

  # Create Login / Logout Functionality ----
    # Login
  output$login_link <- renderUI({
    if(is.null(login_status)) {
      actionLink('login_link', '[Sign-In]', style = 'color: white;') #icon = icon('sign-in-alt'), 
    } 
  })  

  # User Information ----
  makeReactiveBinding("login_status")
  user_info <- reactive({
    httr::content(login_status, "parsed", encoding = "UTF-8")[[3]]
    })
  
    # Modal
  modal_widgits <- list(
    textInput('username','Username', width = "100%"), 
    passwordInput('password', 'Password', width = "100%"), 
    tags$head(tags$script(HTML(jscode))),
    actionButton('login', 'Login'))
  
  observeEvent(input$login_link,
               showModal(modalDialog(
                 modal_widgits,
                 easyClose = TRUE,
                 title = "DFRM Fisheries Data Access",
                 footer = HTML("<em> Restricted data access is intended for DFRM Staff only. </em>")
               ))
  )
  observeEvent(input$login, {
    
    if(input$username == '' | input$password == '')
    {
      showModal(modalDialog(
        modal_widgits,
        easyClose = TRUE,
        title = "Username or password fields are blank.",
        footer = "Please fill in your username and password correctly."
      ))
    } else {
      login_status <<- cdmsLogin(input$username, input$password, cdms_host = cdms_host) 
      
      if(status_code(login_status) != 200) {
        showModal(modalDialog(
          modal_widgits,
          easyClose = TRUE,
          title = "Invalid Username or Password",
          footer = "I'm sorry you are having trouble. Try re-checking the post-it note on your computer screen. :)"
        ))
      } else {
        removeModal()
        # Show restricted Data:
        output$rd_cdms <- renderMenu({menuSubItem('CDMS Datasets', tabName = 'tab_cdms')})
        output$rd_customquery <- renderMenu({menuSubItem('Custom Queries', tabName = 'tab_custom')})
        output$rd_reports <- renderMenu({menuSubItem('Reports', tabName = 'tab_reports')})
        output$rd_files <- renderMenu({menuSubItem('Files', tabName = 'tab_files')})
        
        output$login_link <- renderUI({
          actionLink('greeting', label = paste0('Hello, ', user_info()$Fullname, "!"), style = 'color: white;')
        })
      }
    }
  })
  
  # Home Tab / Leaflet ----
  getPage <- function() { return(includeHTML("./www/kus_map.html")) }
  
  output$map<-renderUI({getPage()})
  
  # # Documents Tab ----
  # observeEvent(input$tabs, {
  #   if(input$tabs == 'tab_documents'){
  #     # build files_table
  #     files <- getAllFiles(cdms_host) %>% 
  #       select(ProjectId, Fullname, Name, Title, Description, Link, FileType)
  #     
  #     projects <- getProjects(cdms_host) %>% 
  #       select(Id, Project = Name)
  #     
  #     documents_df <- left_join(files, projects, by = c('ProjectId'='Id')) %>%
  #       select(Project, Author=Fullname, Title, FileName=Name, Description, Link, FileType)
  # 
  #     # UI
  #     output$documents_info <- renderUI({
  #       tagList(
  #         hr(),
  #         fluidRow(
  #           column(4, offset = 2,
  #                  selectInput(inputId = 'doc_filetype', 'Filter by File Type:',
  #                              choices = c('All Files', unique(documents_df$FileType)), selected = 'All Files'),
  #                  selectInput(inputId = 'doc_project', label = 'Filter by Project:', 
  #                              choices = c('All Projects', unique(documents_df$Project)), selected = 'All Projects')
  #                  ),
  #           column(4, offset = 0,
  #                  selectInput(inputId = 'doc_author', label = 'Filter by Author:',
  #                              choices = c('All Authors', unique(documents_df$Author)), selected = 'All Authors'),
  #                  textInput(inputId = 'doc_keywords', label = 'Description Keyword Search:', 
  #                            placeholder = 'Search description for key words?')
  #                  )
  #         ),
  #         fluidRow(
  #           column(4, offset = 4,
  #                  selectInput(inputId = 'doc_choice', label = 'Select File to Download:', 
  #                                        choices = c('', unique(documents_df$Title)), selected = '')
  #                  ),     
  #           column(2, offset = 5,
  #                  # downloadButton("document_export", label = "Download Document", width = '100%'),
  #                  actionButton(inputId = 'doc_DL', label = "Download Document")
  #                  )
  #         ), hr()
  #       )
  #     })
  # 
  #     
  #     # Documents Table ----
  #     output$documents_table <- DT::renderDataTable({
  #       
  #       cdms_doc_data <<- documents_df %>%
  #         filter(if(input$doc_filetype == 'All Files') FileType %in% unique(documents_df$FileType) else FileType == input$doc_filetype,
  #                if(input$doc_project == 'All Projects') Project %in% unique(documents_df$Project) else Project == input$doc_project,
  #                if(input$doc_author == 'All Authors') Author %in% unique(documents_df$Author) else Author == input$doc_author,
  #                if(input$doc_keywords == '') is.character(Description) else str_detect(Description, input$doc_keywords))
  #                                           # this 'is.character()' is a bit of a hack
  #       
  #       updateSelectInput(session, inputId= 'doc_choice', label= 'Select File to Download:', 
  #                         choices= c('', sort(unique(cdms_doc_data$Title))), selected = '') 
  #       
  #       DT::datatable(cdms_doc_data %>% select(-Link, -FileName, -FileType), options = list(orderClasses = TRUE), filter = 'top')
  #     })
  # 
  #   } # closes 'if'
  # })
  # 
  # # Selected File Info ----
  # observeEvent(input$doc_choice, {
  #   if(input$doc_choice == '') { NULL } else {
  #     docRecord <<- which(grepl(input$doc_choice, cdms_doc_data$Title))
  #     docType <<- cdms_doc_data$FileType[docRecord]
  #     docLink <<- cdms_doc_data$Link[docRecord]
  #     docName <<- cdms_doc_data$FileName[docRecord]
  #     docURL <<- paste0('http:',docLink)
  #     docPath <<- paste0('../',docName)
  #   }
  # })
  # 
  # observeEvent(input$doc_DL, {
  #   if(exists('docName') == FALSE) { NULL } else {
  #   download.file(docURL, destfile = docName, method = 'auto', mode = "wb")
  #   }
  # })
  # 
  # # Document Download ----
  # # output$document_export <- downloadHandler(
  # #       filename = function() {
  # #         paste0(docName)  # FileName as it exists on the server.
  # #       },
  # #       content = function(file) {
  # #         # do we need to if/else based on file type here and have several operations for content?
  # #         download.file(docURL, destfile = docName, method = 'auto', mode = "wb")
  # #      },
  # #       contentType = NULL
  # # )

  # Spawning Ground Surveys Summaries Tab ----
    # UI
  output$sgs_data_button <- renderUI({
    tagList(
      fluidRow(
        column(12, actionButton(inputId= 'sgs_dataload', label = 'Click to Load Data', icon = icon('hourglass-start'), width = '100%'))
      ),
      helpText(HTML('<em> *Initial data load may take several minutes.</em>'))
    )
  })
  
  # Load SGS (Redd & Carcass) data
  observeEvent(input$sgs_dataload, {
    disable(id = 'sgs_dataload')

    # Load Summarized SGS Data 
    sgs_summary_df <<- summariseSGS()

    sgs_pop_list_full <<- sgs_summary_df %>%
      group_by(SpeciesRun, POP_NAME) %>%
      filter(POP_NAME != 'NA') %>%
      dplyr::distinct(POP_NAME) %>%
      arrange(POP_NAME)

    output$sgs_species <- renderUI({
      selectInput(inputId= 'sgs_species', label= 'Choose Species:', choices= as.list(unique(sgs_pop_list_full$SpeciesRun)), selectize= FALSE,
                       selected = 'Spring/summer Chinook salmon', multiple = FALSE)
    })

    hide(id= 'sgs_data_button')
  })

  observeEvent(input$sgs_species, {
    
    sgs_population_list <- sgs_pop_list_full %>%
      filter(SpeciesRun == input$sgs_species) %>%
      pull(POP_NAME)
  
    output$sgs_pop_name <- renderUI({
      selectInput(inputId= 'sgs_pop_name', label= 'Choose Population:', choices= sgs_population_list, selectize= FALSE, multiple = TRUE,
                  selected= NULL)
    })
  })

  observeEvent(input$sgs_pop_name, {
    
    RV$sgs_data <- sgs_summary_df %>%
      filter(SpeciesRun == input$sgs_species,
             POP_NAME %in% input$sgs_pop_name) %>%
      group_by(POP_NAME)
    
    # Total Redds per Year
    output$p_redds <- renderPlotly({
      
      redd_tmp <- RV$sgs_data %>%
        filter(!is.na(TotalRedds))
     
      shiny::validate(
        need(nrow(redd_tmp) > 0, message = '*No Redd data for the current selection.')
      )
      # plot
      yr_plotly <- plot_ly(data = redd_tmp,
                           x = ~Year,
                           y = ~TotalRedds,
                           name = ~POP_NAME,
                           text = ~POP_NAME,
                           hovertemplate = paste(
                             '%{text}<br>',
                             '%{x} %{yaxis.title.text}: %{y}'),
                           type = 'scatter',
                           mode = 'lines+markers',
                           color = ~POP_NAME,
                           colors = viridis_pal(option="D")(length(unique(redd_tmp$POP_NAME)))
                           ) %>%
        layout(title = list(text = '<b>Yearly Total Redd Counts</b>', font = plotly_font),
               yaxis = list(title= 'Total Redds', titlefont = plotly_font),
               xaxis = list(title= 'Spawn Year', titlefont = plotly_font))
    })

    # Base Plotly: %F, pHOS, PSM
    graph_field <- paste('pHOS') # place holder (below base plot requires it until fed to renderPlot())
    p_sgs_base <- plot_ly(data = RV$sgs_data %>% filter(!is.na(graph_field)),
                          name = ~POP_NAME,
                          type = 'box',
                          hoverinfo = 'y',
                          color = ~POP_NAME,
                          colors = viridis_pal(option="D")(length(unique(RV$sgs_data$POP_NAME))),
                          showlegend = FALSE) %>%
      layout(title = list(font = plotly_font),
             xaxis = list(title = ''),
             yaxis = list(titlefont = plotly_font,
                          tickformat = "%",
                          range = c(0,1.05) )) #,zeroline = FALSE

    # SGS Summary - Percent Females
    output$p_females <- renderPlotly({

      graph_field <- paste("PercentFemales")

      shiny::validate(
        need(nrow(RV$sgs_data %>% filter(!is.na(PercentFemales))) > 0, message = '*No data for the current selection.')
      )
      # plot
      pfem_plotly <- p_sgs_base %>%
        add_boxplot(y = ~PercentFemales,
                    x = ~POP_NAME,
                    name = ~ POP_NAME) %>%
        layout(title = list(text = '<b>Percent Females</b>'),
               yaxis= list(title= 'Percent Females'))
    })

    # SGS Summary - Percent Hatchery Origin Spawners
    output$p_phos <- renderPlotly({
      
      graph_field <- paste("pHOS")

      shiny::validate(
        need(nrow(RV$sgs_data %>% filter(!is.na(pHOS))) > 0, message = '*No data for the current selection.')
      )
      # plot
      phos_plotly <- p_sgs_base %>%
        add_boxplot(x = ~POP_NAME,
                y = ~pHOS) %>%
        layout(title = list(text = '<b>Percent Hatchery Origin Spawners</b>'),
               yaxis= list(title= 'pHOS'))
    })

    # SGS Summary - Prespawn Mortalities
    output$p_psm <- renderPlotly({

      graph_field <- paste("PrespawnMortality")

      shiny::validate(
        need(nrow(RV$sgs_data %>% filter(!is.na(PrespawnMortality))) > 0, message = '*No data for the current selection.')
      )
      # plot
      psm_plotly <- p_sgs_base %>%
        add_boxplot(x = ~POP_NAME,
                    y = ~PrespawnMortality) %>%
        layout(title = list(text = '<b>Percent Prespawn Mortality</b>'),
               yaxis= list(title= 'Prespawn Mortality'))
    })

  }) # close observeEvent(input$sgs_pop_name...
  
  # SGS Summary Data Table (reactive)
  output$sgs_table <- DT::renderDataTable({
    
    shiny::validate(
      need(RV$sgs_data, message = '    Table will populate after data load.')
    )
    
    sgs_table_data <<- RV$sgs_data

    DT::datatable(sgs_table_data, options = list(orderClasses = TRUE), filter = 'top')
  })
  
    # SGS_Summary Dataset EXPORT
  output$sgs_export <- downloadHandler(
    filename = function() {
      paste0("NPT_SGS_summary_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(sgs_table_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  # Weir Collections Summaries Tab ----
  # In-Stream Array Abundance Summaries Tab ----
  # Juvenile Monitoring Summaries Tab ----
    
    # UI
  output$juv_data_button <- renderUI({
    tagList(
      fluidRow(
        column(12, actionButton(inputId= 'juv_dataload', label = 'Click to Load Data', icon = icon('hourglass-start'), width = '100%'))
      ),
      helpText(HTML('<em> *Initial data load may take several minutes.</em>'))
    )
  })
  
  # Load Juvenile Summary data (Abundance & Survival)
  observeEvent(input$juv_dataload, {
    disable(id = 'juv_dataload')
    
    juv_summary_df <<- summariseRST()[[1]]  # snag summary dataframe
    
    juv_pop_list_full <<- juv_summary_df %>%
      group_by(SpeciesRun, POP_NAME) %>%
      filter(POP_NAME != 'NA') %>%
      dplyr::distinct(POP_NAME) %>%
      arrange(POP_NAME)
    
    output$juv_species <- renderUI({
      selectInput(inputId= 'juv_species', label= 'Choose Species:', choices= as.list(unique(juv_pop_list_full$SpeciesRun)), selectize= FALSE, 
                  selected = 'Spring/summer Chinook salmon', multiple = FALSE)
    })
    
    hide(id= 'juv_data_button')
  })
  
  observeEvent(input$juv_species, {
    
    juv_population_list <- juv_pop_list_full %>%
      filter(SpeciesRun == input$juv_species) %>%
      pull(POP_NAME)
    
    output$juv_pop_name <- renderUI({
      selectInput(inputId= 'juv_pop_name', label= 'Choose Population:', choices= juv_population_list, selectize= FALSE, 
                  selected = NULL, multiple = TRUE)
    })
    
  })
  
  observeEvent(input$juv_pop_name, {
    
    RV$juv_data <<- juv_summary_df %>%
      filter(SpeciesRun == input$juv_species,
             POP_NAME %in% input$juv_pop_name)
    
    # Natural Juvenile Abundance - Smolts
    output$j_abundance <- renderPlotly({
      
      ja_df <- RV$juv_data %>%
        filter(Origin == 'Natural',
               !is.na(Abundance)) %>%
        group_by(SpeciesRun, Origin, POP_NAME, LocationLabel) %>%
        arrange(POP_NAME, MigratoryYear, Origin)
      
      shiny::validate(
        need(nrow(ja_df) > 0, message = '*No data for the current selection.')
      )
      
      ja_plotly <- plot_ly(data = ja_df,
                           x = ~MigratoryYear,
                           y = ~Abundance,
                           error_y= list(type = 'data',
                                         symmetric = FALSE,
                                         array = ~Ab_U95_errorbar,
                                         arrayminus = ~Ab_L95_errorbar),
                           type = 'scatter',
                           mode = 'lines+markers', 
                           hovertemplate = paste(
                             '%{text}<br>',
                             '%{x} %{yaxis.title.text}: %{y}'),
                           text = ~LocationLabel,
                           color = ~POP_NAME,
                           colors = viridis_pal(option="D")(length(unique(ja_df$POP_NAME)))
                           ) %>%
        layout(title = list(text = '<b>Natural Origin Smolt Abundance</b>',
                            font = plotly_font),
               yaxis= list(hoverformat= ',.',
                           title = 'Abundance',
                           titlefont = plotly_font),
               xaxis= list(title = 'Migratory Year',
                           titlefont = plotly_font)
               )
    })
    
    # Natural Juvenile Survival - Smolts
    output$j_survival <- renderPlotly({
      js_df <- RV$juv_data %>%
        filter(!is.na(Survival)) %>%
        group_by(SpeciesRun, Origin, POP_NAME, LocationLabel, ReleaseGroup) %>%
        arrange(POP_NAME, MigratoryYear, Origin)
      
      shiny::validate(
        need(nrow(js_df) > 0, message = '*No data for the current selection.')
      )

      js_plotly <- plot_ly(data = js_df %>% filter(Origin == 'Natural'),
                           x = ~MigratoryYear,
                           y = ~Survival,
                           error_y= list(type = 'data',
                                         symmetric = FALSE,
                                         array = ~Surv_U95_errorbar,
                                         arrayminus = ~Surv_L95_errorbar),
                           type = 'scatter',
                           mode = 'lines+markers',
                           legendgroup = ~LocationLabel,
                           hovertemplate = paste(
                             '%{text}<br>',
                             'Natural Origin: %{y}'),
                           text = ~LocationLabel,
                           color = ~LocationLabel,
                           colors = viridis_pal(option="D")(length(unique(js_df$POP_NAME)))
                           ) %>%
        add_trace(data = js_df %>% filter(Origin == 'Hatchery'),
                  line = list(dash = 'dot'),
                  hovertemplate = paste(
                    '%{text}<br>', 
                    'Hatchery Origin: %{y}')) %>%
        layout(title = list(text = '<b>Natural and Hatchery Origin Smolt Survival to Lower Granite Dam by Release Site</b>',
                            font = plotly_font),
               yaxis= list(tickformat = "%",
                           range = c(0, 1.05),
                           title = 'Survival',
                           titlefont = plotly_font),
               xaxis= list(title = 'Migratory Year',
                           titlefont = plotly_font))
    })
    
    # Natural Juvenile Equivalents - Smolts
    output$j_equivalents <- renderPlotly({
      je_df <- RV$juv_data %>%
        filter(Origin == 'Natural',
               !is.na(Equivalents)) %>%
        group_by(SpeciesRun, Origin, POP_NAME, LocationLabel)
      
      shiny::validate(
        need(nrow(je_df) > 0, message = '*No data for the current selection.')
      )
      
      je_plotly <- plot_ly(data = je_df,
                           x = ~MigratoryYear,
                           y = ~Equivalents,
                           text = ~LocationLabel,
                           hovertemplate = paste(
                             '%{text}<br>',
                             '%{x} %{yaxis.title.text}: %{y}'),
                           type = 'scatter',
                           mode = 'lines+markers',
                           color = ~POP_NAME,
                           colors = viridis_pal(option="D")(length(unique(je_df$POP_NAME)))
                           ) %>%
        layout(title = list(text = '<b>Natural Origin Smolt Equivalents at Lower Granite Dam</b>',
                            font = plotly_font),
               yaxis= list(hoverformat= ',.',
                           title = 'Smolt Equivalents',
                           titlefont = plotly_font),
               xaxis= list(title = 'Migratory Year',
                           titlefont = plotly_font))
    })
  
  })
  
    # Juvenile Summary Data Table (reactive)
  output$juv_table <- DT::renderDataTable({
    
    shiny::validate(
      need(RV$juv_data, message = '    Table will populate after data load.')
    )
    
    juv_table_data <<- RV$juv_data %>%
      select(POP_NAME, LocationLabel, SpeciesRun, Origin, BroodYear, MigratoryYear, Lifestage, Abundance, Ab_SE, 
             Ab_L95, Ab_U95, ReleaseGroup, SurvivalTo, Survival, Surv_SE, Surv_L95, Surv_U95)
    
    DT::datatable(juv_table_data, options = list(orderClasses = TRUE), filter = 'top')
  })
  
    # Juvenile Summary Dataset EXPORT
  output$juv_export <- downloadHandler(
    filename = function() {
      paste0("NPT_Juvenile_summary_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(juv_table_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # Age Sampling Tab ----
  
  # UI
  output$age_data_button <- renderUI({
    tagList(
      fluidRow(
        column(12, actionButton(inputId= 'age_dataload', label = 'Click to Load Data', icon = icon('hourglass-start'), width = '100%'))
      ),
      helpText(HTML('<em> *Initial data load may take several minutes.</em>'))
    )
  })
  
  # Load Summarized Age data
  observeEvent(input$age_dataload, {
    shinyjs::disable(id='age_summary_btn')
  
  age_summary_bundle <<- summariseAGE()  # feeds our age summary graphs
  
  age_summary_df <<- age_summary_bundle[[1]]
  
  age_pop_list_full <<- age_summary_df %>%
    group_by(SpeciesRun, POP_NAME) %>%
    filter(POP_NAME != 'NA') %>%
    dplyr::distinct(POP_NAME) %>%
    arrange(POP_NAME)
  
  output$age_species <- renderUI({
    selectInput(inputId= 'age_species', label= 'Choose Species:', choices= as.list(unique(age_pop_list_full$SpeciesRun)), selectize= FALSE, 
                selected = 'Spring/summer Chinook salmon', multiple = FALSE)
  })
  
  shinyjs::hide(id= 'age_data_button')
  
  })

  observeEvent(input$age_species, {
    
    age_population_list <- age_pop_list_full %>%
      filter(SpeciesRun == input$age_species) %>%
      pull(POP_NAME)
    
    output$age_pop_name <- renderUI({
      selectInput(inputId= 'age_pop_name', label= 'Choose Population:', choices= age_population_list, selectize= FALSE, 
                  selected = NULL, multiple = TRUE)
    })
    
  })
  
  observeEvent(input$age_pop_name, {
    # Base Plotly - Best (Total), Stream, and Ocean Age
    p_age_base <<- plot_ly(type = 'bar',
                           orientation = 'h',
                           hovertemplate = paste('%{x}'),
                           marker = list(color = ~colors)
                           ) %>%
      layout(title = list(font = plotly_font),
             barmode = 'stack',
             xaxis = list(title = 'Percent of Total',
                          titlefont = plotly_font,
                          tickformat = "%",
                          range = c(0,1.05)),
             yaxis = list(title = ''))
    
      # Best (Total) Age Graphs
    output$n_age_total <- renderPlotly({
      tmp_dat <- age_summary_bundle[[2]] %>%
        filter(SpeciesRun == isolate(input$age_species),
               Origin == 'Natural',
               POP_NAME %in% input$age_pop_name)
      # NATURAL
      shiny::validate(
        need(nrow(tmp_dat) > 0, message = '*No data for the current selection.')
      )

        p_bestage <- p_age_base %>%
          add_bars(data = tmp_dat,
                   x = ~BestAge_Wmean,
                   y = ~POP_NAME,
                   name = ~BestAge) %>%
          layout(title = list(text = '<b>Natural Origin Total Ages</b>'))
    })

    output$h_age_total <- renderPlotly({
      tmp_dat <- age_summary_bundle[[2]] %>%
        filter(SpeciesRun == isolate(input$age_species),
               Origin == 'Hatchery',
               POP_NAME %in% input$age_pop_name)
      # HATCHERY
      shiny::validate(
        need(nrow(tmp_dat) > 0, message = '*No data for the current selection.')
      )

      p_bestage <- p_age_base %>%
        add_bars(data = tmp_dat,
                 x = ~BestAge_Wmean,
                 y = ~POP_NAME,
                 name = ~BestAge) %>%
        layout(title = list(text = '<b>Hatchery Origin Total Ages</b>'))
    })

    # Ocean Age Graphs
    output$n_age_ocean <- renderPlotly({
      tmp_dat <- age_summary_bundle[[3]] %>%
        filter(SpeciesRun == isolate(input$age_species),
               Origin == 'Natural',
               POP_NAME %in% input$age_pop_name)
      # NATURAL
      shiny::validate(
        need(nrow(tmp_dat) > 0, message = '*No data for the current selection.')
      )

        p_oceanage <- p_age_base %>%
          add_bars(data = tmp_dat,
                   x = ~OceanAge_Wmean,
                   y = ~POP_NAME,
                   name = ~OceanAge) %>%
          layout(title = list(text = '<b>Natural Origin Ocean Ages</b>'))
    })

    output$h_age_ocean <- renderPlotly({
      tmp_dat <- age_summary_bundle[[3]] %>%
        filter(SpeciesRun == isolate(input$age_species),
               Origin == 'Hatchery',
               POP_NAME %in% input$age_pop_name)
      # HATCHERY
      shiny::validate(
        need(nrow(tmp_dat) > 0, message = '*No data for the current selection.')
      )

      p_oceanage <- p_age_base %>%
        add_bars(data = tmp_dat,
                 x = ~OceanAge_Wmean,
                 y = ~POP_NAME,
                 name = ~OceanAge) %>%
        layout(title = list(text = '<b>Hatchery Origin Ocean Ages</b>'))
    })

    # Stream Age Graphs
    output$n_age_stream <- renderPlotly({
      tmp_dat <- age_summary_bundle[[4]] %>%
        filter(SpeciesRun == isolate(input$age_species),
               Origin == 'Natural',
               POP_NAME %in% input$age_pop_name)
      # NATURAL
      shiny::validate(
        need(nrow(tmp_dat) > 0, message = '*No data for the current selection.')
      )

      p_streamage <- p_age_base %>%
        add_bars(data = tmp_dat,
                 x = ~StreamAge_Wmean,
                 y = ~POP_NAME,
                 name = ~StreamAge) %>%
        layout(title = list(text = '<b>Natural Origin Stream Ages</b>'))
    })

    output$h_age_stream <- renderPlotly({
      tmp_dat <- age_summary_bundle[[4]] %>%
        filter(SpeciesRun == isolate(input$age_species),
               Origin == 'Hatchery',
               POP_NAME %in% input$age_pop_name)
      # HATCHERY
      shiny::validate(
        need(nrow(tmp_dat) > 0, message = '*No data for the current selection.')
      )

      p_streamage <- p_age_base %>%
        add_bars(data = tmp_dat,
                 x = ~StreamAge_Wmean,
                 y = ~POP_NAME,
                 name = ~StreamAge) %>%
        layout(title = list(text = '<b>Hatchery Origin Stream Ages</b>'))
    })
    
  })
  
  # Restricted Data Access ====================================================
  # CDMS DATASETS ----
    # Clear Field Values Button
  observeEvent(input$clear_fields, {
    if(exists('raw_dat') != TRUE) {
      NULL
    } else {
    RV$query_data <<- raw_dat 
    
      updateSelectInput(session, inputId= 'q_species', label= 'Choose Species:', choices= sort(unique(RV$query_data$SpeciesRun)),
                        selected = NULL) 
      updateSelectInput(session, inputId= 'q_pop_name', label= 'Choose Population:', choices= sort(unique(RV$query_data$POP_NAME)),
                        selected = NULL) 
      updateSelectInput(session, inputId= 'q_stream', label= 'Choose Stream:', choices= sort(unique(RV$query_data$StreamName)),
                        selected = NULL) 
      updateSelectInput(session, inputId= 'q_fields', label= 'Choose Fields in Desired Order:', choices= names(RV$query_data), selected = NULL) 
      
      updateSliderInput(session, inputId = 'q_year', label = 'Choose Years*:', min = min(RV$query_data$Year), max = max(RV$query_data$Year), 
                        value = c(min(RV$query_data$Year), max(RV$query_data$Year)), step = 1)
    }
  })
  
  # Gather/Create dataset list from CDMS ----
  if(html_code == 200){
    datasets <- getDatastores(cdms_host = cdms_host) %>%
      rename(DatastoreId = Id, DatastoreName = Name)
  }
  
  output$raw_dataset_menu <- renderUI({
    
    dataset <<- datasets %>%
      select(DatastoreId, DatastoreName) %>%
      distinct(DatastoreId, .keep_all = TRUE) %>%
      filter(!DatastoreId %in% c(81:84, 87:91, 93, 98:99)) %>% 
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

    raw_dat <<- getDatasetView(input$datasets, cdms_host = cdms_host) %>%
      mutate(SpeciesRun = gsub(' NA', '', paste(Species, Run)))

      RV$query_data <<- raw_dat  # reactive dataframe
      
      # Update our Inputs
      updateSelectInput(session, inputId= 'q_species', label= 'Choose Species:', choices= sort(unique(RV$query_data$SpeciesRun)),
                        selected = NULL) 
      updateSelectInput(session, inputId= 'q_pop_name', label= 'Choose Population:', choices= sort(unique(RV$query_data$POP_NAME)),
                        selected = NULL) 
      updateSelectInput(session, inputId= 'q_stream', label= 'Choose Stream:', choices= sort(unique(RV$query_data$StreamName)),
                        selected = NULL) 
      updateSelectInput(session, inputId= 'q_fields', label= 'Choose Fields in Desired Order:', choices= names(RV$query_data), selected = NULL) 
      
      updateSliderInput(session, inputId = 'q_year', label = '*Choose Years:', min = min(RV$query_data$Year), max = max(RV$query_data$Year), 
                        value = c(min(RV$query_data$Year), max(RV$query_data$Year)), step = 1)

      # Display loaded CDMS Dataset
      output$selected_cdms <- renderText({
        selected_df <- dataset %>%
          filter(DatastoreId == isolate(input$datasets)) %>%
          pull(DatastoreName)
        
        paste0(h2('Currently Loaded Dataset: ', selected_df))
      })

        enable(id = 'raw_submit')
        enable(id = 'datasets')
  })
  
  # Input Reactivity ----
    # Species
  observeEvent(input$q_species, {
                   
                   # save existing input values
                   selected_pop <- input$q_pop_name
                   selected_stream <- input$q_stream

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

                   updateSelectInput(session, inputId= 'q_pop_name', label= 'Choose Population:', choices= sort(unique(RV$query_data$POP_NAME)),
                                     selected = selected_pop)
                   updateSelectInput(session, inputId= 'q_stream', label= 'Choose Stream:', choices= sort(unique(RV$query_data$StreamName)),
                                     selected = selected_stream)
                 })
    # POP
  observeEvent(input$q_pop_name, {
                   
                   # save existing input values
                   selected_species <- input$q_species
                   selected_stream <- input$q_stream
                   
                   RV$query_data <<- raw_dat  # re-Populate our reactive dataframe.
                   
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

                   updateSelectInput(session, inputId= 'q_species', label= 'Choose Species:', choices= sort(unique(RV$query_data$SpeciesRun)),
                                     selected = selected_species)
                   updateSelectInput(session, inputId= 'q_stream', label= 'Choose Stream:', choices= sort(unique(RV$query_data$StreamName)),
                                     selected = selected_stream)
                 })
    # Stream
  observeEvent(input$q_stream, {
                   
                   # save existing input values
                   selected_species <- input$q_species
                   selected_pop <- input$q_pop_name

                   RV$query_data <<- raw_dat  # re-Populate our reactive dataframe.
                   
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

                   updateSelectInput(session, inputId= 'q_species', label= 'Choose Species:', choices= sort(unique(RV$query_data$SpeciesRun)),
                                     selected = selected_species)
                   updateSelectInput(session, inputId= 'q_pop_name', label= 'Choose Population:', choices= sort(unique(RV$query_data$POP_NAME)),
                                     selected = selected_pop)
                 })

  # Create CDMS Dataset table (reactive/self-updating) ----
  output$raw_table <- DT::renderDataTable({
    
    shiny::validate(
      need(RV$query_data, message = '    Table will populate after data load.')
    )

    if(is.null(input$q_fields)) {
      cdms_table_data <<- RV$query_data %>%
        filter(Year %in% c(min(input$q_year): max(input$q_year)))
      
    } else {
      cdms_table_data <<- RV$query_data %>% 
        filter(Year %in% c(min(input$q_year): max(input$q_year))) %>%
        select(input$q_fields)
      }

    DT::datatable(cdms_table_data, options = list(orderClasses = TRUE), filter = 'top')
  })
  
    # CDMS Dataset EXPORT ----
  output$raw_export <- downloadHandler(
    filename = function() {
      paste0(input$datasets,"_raw_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(cdms_table_data, file, row.names = FALSE, na='')
    },
    contentType = "text/csv"
  )
  
  # Custom Queries (CUSTOM!) --------------------------------------------------
  
  # Dynamic Description for Custom Queries
  output$query_description <- renderText({
    # match Query with Description and paste value
    q_description <- custom_query_df$query_descriptions[match(input$custom_query_menu, custom_query_df$query_names)]
    paste0("Description: ", q_description)
  })

  # Submit Custom Query Request
  observeEvent(input$custom_submit, {
    
    if(input$custom_query_menu == '-Select Custom Query-') {
      NULL
    } else {
      
      disable(id='custom_query_menu')
      disable(id='custom_submit')

      if(input$custom_query_menu == 'RST Summary') {
        RV$cq_data <<- summariseRST()[[2]]
      } else {
        if(input$custom_query_menu == 'Fall Chinook Redd Summary') {
          RV$cq_data <<- sum_FCHN_redds() 
        } else{
          RV$cq_data <<- summariseSGS() #'SGS Summary'
          }
      }
    
      updateSelectInput(session, inputId= 'cq_fields', label= 'Choose Fields in Desired Order:', choices= names(RV$cq_data), selected = NULL) 

      # Display loaded Custom Query
      output$selected_custom <- renderText({
        paste0(h2('Currently Loaded Custom Query: ', isolate(input$custom_query_menu)))
      })
      
    enable(id='custom_query_menu')
    enable(id='custom_submit')
    }
  })
  
  # Apply Field Selection and create Custom Query Table ----
  output$custom_table <- DT::renderDataTable({
    
    if(is.null(input$cq_fields)) {
      custom_table_data <<- RV$cq_data 
    } else {
      custom_table_data <<- RV$cq_data %>% 
        select(input$cq_fields)
    }
    
    DT::datatable(custom_table_data, options = list(orderClasses = TRUE), filter = 'top')  
  })
  
  # Custom Query EXPORT
  output$custom_export <- downloadHandler(
    filename = function() {
      paste0(input$custom_query_menu,"_custom_query_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(custom_table_data, file, row.names = FALSE, na = '')
    },
    contentType = "text/csv"
  )
  
  # Reports ----
    # PDF 
  output$reports <- downloadHandler(

    # filename = 'test.pdf',
    filename = function(){
      paste0(gsub(" ","_",input$pdf_reports),
             "_",
             format(Sys.time(), "%m_%d_%y_%H%M%S"),
             ".pdf")
    },
    
    content = function(file){

            # Dynamic File Path based on input$pdf_reports
      if(input$pdf_reports == 'Juvenile Summary MY17') {
        tempReport <- file.path(getwd(), "JUV_Status_Report.Rmd")

        } else {
          tempReport <- file.path(getwd(), "SGS_Status_Report.Rmd")
      
          }
      
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
      
    }
  )
  
} # close Server
