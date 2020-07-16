# Kus Server ----
server <- function(input, output, session) {

  showModal(modalDialog(
    title = "Kus Data Use Agreement",
    'The Kus web application is intended to provide near real-time data summaries and visualizations of Snake River Basin anadroumous fish monitoring activities for Nez Perce Tribal Members, Department of Fisheries Resources Management staff and the general public. The data provided is preliminary and subject to change. Before using or publishing any data provided in this application you must contact NPT Data Management staff to obtain verification, data use limitations, metadata, and the proper citation format.',
    hr(),
    "The data available in this application is updated daily and is representative of what was stored in the Nez Perce Tribe's Centralized Database Management System at the time indicated by the Data Version, displayed in the bottom left corner.",
    footer = modalButton('I Agree')
  ))
  
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
        output$rd_fins <- renderMenu({menuSubItem('FINS Data Access', tabName = 'tab_fins')})
        output$rd_reports <- renderMenu({menuSubItem('Reports', tabName = 'tab_reports')})
        
        output$login_link <- renderUI({
          actionLink('greeting', label = paste0('Hello, ', user_info()$Fullname, "!"), style = 'color: white;')
        })
      }
    }
  })
  
  # Home Tab ----
  window_df <- queryWindowCnts(dam = 'LWG', 
                               spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw','fb'),
                               spawn_yr = year(Sys.Date()),
                               start_day = '01/01',
                               end_day = '12/31') %>%
    gather(key = spp, value = 'Count', -Year, -Date) %>%
    mutate(species = case_when(
      grepl('Chinook', spp) ~ 'Chinook',
      grepl('Coho', spp) ~ 'Coho',
      grepl('Steelhead', spp) ~ 'Steelhead',
      grepl('Sockeye', spp) ~ 'Sockeye'
    )) %>%
    group_by(Year, Date, species) %>%
    summarise(Count = sum(Count)) %>%
    ungroup() %>%
    filter(Date < Sys.Date()) # this isn't perfect. I'm not sure when 'yesterdays' count is added.
    
  
  output$windowChinook <- renderValueBox({
    
    n <- window_df %>%
      filter(species == 'Chinook') %>%
      summarise(n = sum(Count)) %>%
      pull(n)
    
    valueBox(
        value = prettyNum(n, big.mark = ","),
        color = 'aqua',
        icon = icon("fish"),
        subtitle = "Chinook Salmon"
      )
    })
  
  output$windowSteelhead <- renderValueBox({

    n <- window_df %>%
      filter(species == 'Steelhead') %>%
      summarise(n = sum(Count)) %>%
      pull(n)
    
    valueBox(
      value = prettyNum(n, big.mark = ","),
      color = 'aqua',
      icon = icon("fish"),
      subtitle = "Steelhead"
    )
  })
  
  output$windowCoho <- renderValueBox({

    n <- window_df %>%
      filter(species == 'Coho') %>%
      summarise(n = sum(Count)) %>%
      pull(n)
    
    valueBox(
      value = prettyNum(n, big.mark = ","),
      color = 'aqua',
      icon = icon("fish"),
      subtitle = "Coho Salmon"
    )
  })
  
  output$windowSockeye <- renderValueBox({

    n <- window_df %>%
      filter(species == 'green') %>%
      summarise(n = sum(Count)) %>%
      pull(n)
    
    valueBox(
      value = prettyNum(n, big.mark = ","),
      color = 'aqua',
      icon = icon("fish"),
      subtitle = "Sockeye Salmon"
    )
  })
  
  output$window_plot <- renderPlotly({
    plot_ly(data = window_df,
                         x = ~ Date,
                         y = ~ Count,
                         name = ~ species,
                         text = ~ species,
                         hovertemplate = paste(
                           '%{text}<br>',
                           '%{x} %{yaxis.title.text}: %{y}'),
                         type = 'scatter',
                         mode = 'lines',
                         color = ~ species,
                         colors = viridis_pal(option="D")(length(unique(window_df$species)))
    ) %>%
      layout(yaxis = list(title= 'Daily Window Count', titlefont = plotly_font),
             xaxis = list(title= 'Date', titlefont = plotly_font))
  })
  
  #getPage <- function() { return(includeHTML("./www/kus_map.html")) }
  #  output$map<-renderUI({getPage()})
  
  # Production Tab ----
  observeEvent(input$tabs, {
    if(input$tabs == 'tab_production'){
      projects_production <<- getProjects(cdms_host) %>%
        filter(SubProgram == 'Production')  # filter out unwanted projects ??
    } 
    
    output$production_select <- renderUI({
      selectInput('production_select', 'Select Project', choices = sort(unique(projects_production$Name)),
                  selected = 'Snake Basin Steelhead Assessments')
    })
    
  })
  
  # Access below document for list of MetadataPropertyId values.
  # load(file = './data/metadataproperties.rda')  # for list of metadataproperties (#s)
  observeEvent(input$production_select, {
    ProjId <- projects_production[match(input$production_select, projects_production$Name), 1] # get ProjectId
    proj_info <- getProject(ProjId) # get project summary page info (metadata)
    proj_meta <- proj_info[[18]]
    
    # proj_locations <- proj_info[[4]] %>%
    #   distinct(Name, .keep_all = TRUE) %>%
    #   filter(LocationTypeId %in% c(1123, 1124)) %>% # Weirs and RST
    #   select(Name, Latitude, Longitude)
    
    output$production_description <- renderText({
      proj_info[[13]]
    })
    
    output$production_objectives <- renderText({
      proj_meta[match(20, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')]
    })
    
    output$production_PL <- renderText({
      # proj_meta[match(43, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')]
      paste(proj_meta[match(43, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')], " (",
            proj_meta[match(45, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')], ")", sep = '')
    })
    
    output$production_staff <- renderText({
      proj_meta[match(4, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')]
    })
    
  })
  
  # Research Tab ----
  observeEvent(input$tabs, {
    if(input$tabs == 'tab_research'){
      projects_research <<- getProjects(cdms_host) %>%
        filter(SubProgram == 'Research')  # filter out unwanted projects ??
    } 
    
    output$research_select <- renderUI({
      selectInput('research_select', 'Select Project', choices = sort(unique(projects_research$Name)),
                  selected = 'Snake Basin Steelhead Assessments')
    })

  })

  # Access below document for list of MetadataPropertyId values.
    # load(file = './data/metadataproperties.rda')  # for list of metadataproperties (#s)
  observeEvent(input$research_select, {
    ProjId <- projects_research[match(input$research_select, projects_research$Name), 1] # get ProjectId
    proj_info <- getProject(ProjId) # get project summary page info (metadata)
    proj_meta <- proj_info[[18]]
    
    # proj_locations <- proj_info[[4]] %>%
    #   distinct(Name, .keep_all = TRUE) %>%
    #   filter(LocationTypeId %in% c(1123, 1124)) %>% # Weirs and RST
    #   select(Name, Latitude, Longitude)
    
    output$research_description <- renderText({
      proj_info[[13]]
    })
    
    output$research_objectives <- renderText({
      proj_meta[match(20, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')]
    })
    
    output$research_PL <- renderText({
      # proj_meta[match(43, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')]
      paste(proj_meta[match(43, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')], " (",
            proj_meta[match(45, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')], ")", sep = '')
    })
    
    output$research_staff <- renderText({
      proj_meta[match(4, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')]
    })
    
  })
  
  # Documents Tab ----
  observeEvent(input$tabs, {
    if(input$tabs == 'tab_documents'){
      # build files_table
      files <- getAllFiles(cdms_host) %>%
        select(ProjectId, Fullname, Name, Title, Description, Link, FileType)

      projects <- getProjects(cdms_host) %>%
        select(Id, Project = Name)

      documents_df <- left_join(files, projects, by = c('ProjectId'='Id')) %>%
        select(Project, Author=Fullname, Title, FileName=Name, Description, Link, FileType)

      # UI
      output$documents_info <- renderUI({
        tagList(
          hr(),
          fluidRow(
            column(4, offset = 2,
                   selectInput(inputId = 'doc_filetype', 'Filter by File Type:',
                               choices = c('All Files', unique(documents_df$FileType)), selected = 'All Files'),
                   selectInput(inputId = 'doc_project', label = 'Filter by Project:',
                               choices = c('All Projects', unique(documents_df$Project)), selected = 'All Projects')
                   ),
            column(4, offset = 0,
                   selectInput(inputId = 'doc_author', label = 'Filter by Author:',
                               choices = c('All Authors', unique(documents_df$Author)), selected = 'All Authors'),
                   textInput(inputId = 'doc_keywords', label = 'Description Keyword Search:',
                             placeholder = 'Search description for key words?')
                   )
          ),
          fluidRow(
            column(4, offset = 4,
                   selectInput(inputId = 'doc_choice', label = 'Select File to Download:',
                                         choices = c('', unique(documents_df$Title)), selected = '')
                   ),
            column(2, offset = 5,
                   downloadButton("document_export", label = "Download Document", width = '100%')
                   )
          ), hr()
        )
      })


      # Documents Table ----
      output$documents_table <- DT::renderDataTable({

        cdms_doc_data <<- documents_df %>%
          filter(if(input$doc_filetype == 'All Files') FileType %in% unique(documents_df$FileType) else FileType == input$doc_filetype,
                 if(input$doc_project == 'All Projects') Project %in% unique(documents_df$Project) else Project == input$doc_project,
                 if(input$doc_author == 'All Authors') Author %in% unique(documents_df$Author) else Author == input$doc_author,
                 if(input$doc_keywords == '') is.character(Description) else str_detect(Description, input$doc_keywords))
                                            # this 'is.character()' is a bit of a hack

        updateSelectInput(session, inputId= 'doc_choice', label= 'Select File to Download:',
                          choices= c('', sort(unique(cdms_doc_data$Title))), selected = '')

        DT::datatable(cdms_doc_data %>% select(-Link, -FileName, -FileType), options = list(orderClasses = TRUE), filter = 'top')
      })

    } # closes 'if'
  })

  # Selected File Info
  observeEvent(input$doc_choice, {
    if(input$doc_choice == '') { NULL } else {
      docRecord <<- which(grepl(input$doc_choice, cdms_doc_data$Title))
      docType <<- cdms_doc_data$FileType[docRecord]
      docLink <<- cdms_doc_data$Link[docRecord]
      docName <<- cdms_doc_data$FileName[docRecord]
      docURL <<- paste0('http:',docLink)
      docPath <<- paste0('../',docName)
    }
  })

  # Document Download
  output$document_export <- downloadHandler(
        filename = function() {
          paste0(docName)  # FileName as it exists on the server.
        },
        content = function(file) {
          
          GET(docURL, write_disk(file))
       },
        contentType = NULL
  )

  # Spawning Ground Surveys Summaries Tab ----
  observeEvent(input$tabs, {
    if(input$tabs == 'tab_sgs'){

    sgs_pop_list_full <<- SGSsummary %>%
      group_by(SpeciesRun, POP_NAME) %>%
      filter(POP_NAME != 'NA') %>%
      dplyr::distinct(POP_NAME) %>%
      arrange(POP_NAME)

    output$sgs_species <- renderUI({
      selectInput(inputId= 'sgs_species', label= 'Choose Species:', choices= as.list(unique(sgs_pop_list_full$SpeciesRun)), selectize= FALSE,
                       selected = 'Spring/summer Chinook salmon', multiple = FALSE)
    })

    } # close if(input$tabs == 'tab_sgs')
  })

  observeEvent(input$sgs_species, {
    
    sgs_population_list <- sgs_pop_list_full %>%
      filter(SpeciesRun == input$sgs_species) %>%
      pull(POP_NAME)
  
    output$sgs_pop_name <- renderUI({
      selectInput(inputId= 'sgs_pop_name', label= 'Choose Population:', choices= sgs_population_list, selectize= FALSE, multiple = TRUE,
                  selected= 'Big Creek')
    })
  })

  observeEvent(input$sgs_pop_name, {
    
    RV$sgs_data <- SGSsummary %>%
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
      write.csv(sgs_table_data[input[["sgs_table_rows_all"]], ], file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  # Weir Collections Summaries Tab ----
  observeEvent(input$tabs, {
    if(input$tabs == 'tab_weir'){
      
      # list of traps for input
      weir_list <<- p_weir_df %>%  
        group_by(trap, SpeciesRun) %>%
        filter(SpeciesRun %in% c('Spring/summer Chinook', 'Summer Steelhead')) %>%
        distinct(trap) 


      output$weir_species <- renderUI({
        selectInput(inputId= 'weir_species', label= 'Choose Species:', 
                    choices= c('Fall Chinook', 'Spring/summer Chinook', 'Summer Steelhead'), selectize= FALSE, 
                    selected = 'Spring/summer Chinook', multiple = FALSE)
      })
      
      output$weir_sum_chn <- renderDataTable({
        chn_tmp <<- weir_sum_all %>% filter(Species == 'Chinook')
        
        DT::datatable(chn_tmp, options = list(orderClasses = TRUE))
      })
      
      output$weir_sum_sth <- renderDataTable({
        sth_tmp <<- weir_sum_all %>% filter(Species == 'Steelhead')
        
        DT::datatable(sth_tmp, options = list(orderClasses = TRUE))
      })
      
    }
  })
  

  observeEvent(input$weir_species, {
    
    weir_trp <- weir_list %>%
      filter(SpeciesRun == input$weir_species)
    
    output$weir_trap <- renderUI({
      selectInput(inputId= 'weir_trap', label= 'Choose Weir:', choices= sort(unique(weir_trp$trap)), selectize= FALSE, 
                  selected = 'Lostine River Weir', multiple = FALSE)
    })
    
  })
  
  # weir select 
  observeEvent(input$weir_trap, {
    # data
    weir_disp <- cnt_groups(NPTweir, disposition, disposition, trap, species, SpeciesRun, sex, origin, age_designation) %>%
      spread(key = disposition, value = n)

    weir_totals <- cnt_groups(NPTweir, sex, trap, species, SpeciesRun, sex, origin, age_designation) %>%
      rename(TotalCatch = n)

    RV$weir_sum <<- full_join(weir_disp, weir_totals, by = c('trap', 'species', 'SpeciesRun', 'sex', 'origin', 'age_designation'))
    
    # for weir catch plot
    tmp_p_weir_df <- p_weir_df %>%
      filter(SpeciesRun == input$weir_species,
             trap == input$weir_trap)
    
    
    output$weir_year <- renderUI({
      selectInput(inputId = 'weir_year', label = 'Choose Year:', choices = unique(sort(tmp_p_weir_df$trap_year)), 
                  selected = max(tmp_p_weir_df$trap_year))
    })
    
    })
  

    
    # Weir Catch PLOT
  observeEvent(input$weir_year, {
    
    output$p_weircatch <- renderPlotly({
      
      weir_filtered <- p_weir_df %>%
        filter(SpeciesRun == input$weir_species,
               trap == input$weir_trap,
               trap_year == input$weir_year)

      plot_ly(data = weir_filtered,
              x = ~trapped_date,
              y = ~DailyCatch,
              type = 'bar',
              marker = list(line = list(width=0.8, color = 'rgb(0,0,0)')), # outline of bars
              legendgroup = ~origin,
              color = ~origin,
              colors = viridis_pal(option="D")(length(unique(weir_filtered$origin)))) %>%
        layout(hovermode = 'x',
               barmode = 'stack',
               # bargap = .3,
               title = list(text = paste(input$weir_year, input$weir_trap, input$weir_species, 'Daily Catch, by Origin'),
                            font = plotly_font),
               yaxis= list(
                 title = 'Daily Catch',
                 titlefont = plotly_font),
               xaxis= list(title = 'Date',
                           type = 'date',
                           tickformat = '%m/%d/%y',
                           nticks = nrow(weir_filtered)/2,
                           tickangle = -45,
                           titlefont = plotly_font
               ))
    })
  })
  
  # Weir Proportions Table and Plot
  
  output$weir_props_table <- renderDataTable({
    
    weir_props_filtered <- weir_props %>%
      filter(SpeciesRun == input$weir_species,
             trap == input$weir_trap,
             trap_year == input$weir_year) 
    
    DT::datatable(weir_props_filtered, options = list(orderClasses = TRUE))

  })
  
  output$p_weir_props <- renderPlotly({
    
    weir_props_filtered <- weir_props %>%
      filter(SpeciesRun == input$weir_species,
             trap == input$weir_trap,
             trap_year == input$weir_year)
    
    plot_ly(data = weir_props_filtered,
            x = ~Statistic,
            y = ~n,
            type = 'bar',
            marker = list(line = list(width=1, color = 'rgb(0,0,0)')), # outline of bars
            color = ~Statistic,
            colors = viridis_pal(option="D")(length(unique(weir_props_filtered$Statistic))),
            showlegend = FALSE) %>%
      add_text(text=~n, hoverinfo='none', textposition = 'top', showlegend = FALSE,
               textfont=list(size=15, color="black")) %>%
      layout(hovermode = 'x',
             title = list(text = paste(input$weir_year, input$weir_trap, input$weir_species, 'Catch Statistics'),
                          font = plotly_font),
             yaxis= list(
               title = 'Percent of Total (%)',
               tickformat = "%",
               range = c(0,1.05),
               titlefont = plotly_font),
             xaxis= list(title = '',
                         tickangle = -45,
                         titlefont = plotly_font
             ))
  })
  
  
  # Weir Disposition Summary Data Table
  output$weir_table <- DT::renderDataTable({

    shiny::validate(
      need(RV$weir_sum, message = '    Table will populate after data load.')
    )

    weir_table_data <<- RV$weir_sum %>% # is this step necessary?
      filter(trap %in% input$weir_trap)

    DT::datatable(weir_table_data, options = list(orderClasses = TRUE), filter = 'top')
  })
  
  # Weir Disposition Summary EXPORT
  output$weir_export <- downloadHandler(
    filename = function() {
      paste0("NPT_weir_disposition_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(weir_table_data[input[["weir_table_rows_all"]], ], file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # In-Stream Array Abundance Summaries Tab ----
  # Juvenile Monitoring Summaries Tab ----
  observeEvent(input$tabs, {
    if(input$tabs == 'tab_juv'){

    juv_pop_list_full <<- JUVsummary[[1]] %>%
      group_by(SpeciesRun, POP_NAME) %>%
      filter(POP_NAME != 'NA') %>%
      dplyr::distinct(POP_NAME) %>%
      arrange(POP_NAME)
    
    output$juv_species <- renderUI({
      selectInput(inputId= 'juv_species', label= 'Choose Species:', choices= as.list(unique(juv_pop_list_full$SpeciesRun)), selectize= FALSE, 
                  selected = 'Spring/summer Chinook salmon', multiple = FALSE)
    })
    
    }
  })
  
  observeEvent(input$juv_species, {
    
    juv_population_list <- juv_pop_list_full %>%
      filter(SpeciesRun == input$juv_species) %>%
      pull(POP_NAME)
    
    output$juv_pop_name <- renderUI({
      selectInput(inputId= 'juv_pop_name', label= 'Choose Population:', choices= juv_population_list, selectize= FALSE, 
                  selected = "East Fork South Fork Salmon River", multiple = TRUE)
    })
    
  })
  
  observeEvent(input$juv_pop_name, {
    
    RV$juv_data <<- JUVsummary[[1]] %>%
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
      write.csv(juv_table_data[input[["juv_table_rows_all"]], ], file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # Age Sampling Tab ----
  observeEvent(input$tabs, {
    if(input$tabs == 'tab_age'){
  
    age_pop_list_full <<- AGEsummary[[1]] %>%
      group_by(SpeciesRun, POP_NAME) %>%
      filter(POP_NAME != 'NA') %>%
      dplyr::distinct(POP_NAME) %>%
      arrange(POP_NAME)
    
    output$age_species <- renderUI({
      selectInput(inputId= 'age_species', label= 'Choose Species:', choices= as.list(unique(age_pop_list_full$SpeciesRun)), selectize= FALSE, 
                  selected = 'Spring/summer Chinook salmon', multiple = FALSE)
    })
  
    }
  })

  observeEvent(input$age_species, {
    
    age_population_list <- age_pop_list_full %>%
      filter(SpeciesRun == input$age_species) %>%
      pull(POP_NAME)
    
    output$age_pop_name <- renderUI({
      selectInput(inputId= 'age_pop_name', label= 'Choose Population:', choices= age_population_list, selectize= FALSE, 
                  selected = "Big Creek", multiple = TRUE)
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
      tmp_dat <- AGEsummary[[2]] %>%
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
      tmp_dat <- AGEsummary[[2]] %>%
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
      tmp_dat <- AGEsummary[[3]] %>%
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
      tmp_dat <- AGEsummary[[3]] %>%
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
      tmp_dat <- AGEsummary[[4]] %>%
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
      tmp_dat <- AGEsummary[[4]] %>%
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
  
  # Dataset selection ----
  output$raw_dataset_menu <- renderUI({
    
    datasets_ls <- as.list(datasets[,1])
  
    names(datasets_ls) <- datasets[,2]
    selectInput("datasets", label = 'Choose Dataset:', choices = datasets_ls, selected = NULL, selectize = TRUE, width = '100%')
  }) 
  
  observeEvent(input$raw_submit,{

    raw_dat <<- get(x=datasets[match(input$datasets, datasets$DatastoreId), 3])

      RV$query_data <<- get(x=datasets[match(input$datasets, datasets$DatastoreId), 3])
    
      # Update Inputs
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
        selected_df <- datasets %>%
          filter(DatastoreId == isolate(input$datasets)) %>%
          pull(DatastoreName)
        
        paste0(h2('Currently Loaded Dataset: ', selected_df))
      })

  })
  
  # Input Reactivity ----
    # CDMS Dataset: Species
  observeEvent(input$q_species, {
                   
                   # save existing input values (to keep selectInputs the same)
                   selected_pop <- input$q_pop_name
                   selected_stream <- input$q_stream

                   # filter dataframe
                   RV$query_data <<- raw_dat %>%
                   filter(if(is.null(input$q_species)) SpeciesRun %in% unique(raw_dat$SpeciesRun) else SpeciesRun == input$q_species,
                          if(is.null(input$q_pop_name)) POP_NAME %in% unique(raw_dat$POP_NAME) else POP_NAME == input$q_pop_name,
                          if(is.null(input$q_stream)) StreamName %in% unique(raw_dat$StreamName) else StreamName == input$q_stream)
                   
                   updateSelectInput(session, inputId= 'q_pop_name', label= 'Choose Population:', choices= sort(unique(RV$query_data$POP_NAME)),
                                     selected = selected_pop)
                   updateSelectInput(session, inputId= 'q_stream', label= 'Choose Stream:', choices= sort(unique(RV$query_data$StreamName)),
                                     selected = selected_stream)
                 })
    # CDMS Dataset: POP_NAME
  observeEvent(input$q_pop_name, {
                   
                   # save existing input values
                   selected_species <- input$q_species
                   selected_stream <- input$q_stream

                   RV$query_data <<- raw_dat %>%
                     filter(if(is.null(input$q_species)) SpeciesRun %in% unique(raw_dat$SpeciesRun) else SpeciesRun == input$q_species,
                            if(is.null(input$q_pop_name)) POP_NAME %in% unique(raw_dat$POP_NAME) else POP_NAME == input$q_pop_name,
                            if(is.null(input$q_stream)) StreamName %in% unique(raw_dat$StreamName) else StreamName == input$q_stream)

                   updateSelectInput(session, inputId= 'q_species', label= 'Choose Species:', choices= sort(unique(RV$query_data$SpeciesRun)),
                                     selected = selected_species)
                   updateSelectInput(session, inputId= 'q_stream', label= 'Choose Stream:', choices= sort(unique(RV$query_data$StreamName)),
                                     selected = selected_stream)
                 })
    # CDMS Dataset: StreamName
  observeEvent(input$q_stream, {
                   
                   # save existing input values
                   selected_species <- input$q_species
                   selected_pop <- input$q_pop_name

                   RV$query_data <<- raw_dat %>%
                     filter(if(is.null(input$q_species)) SpeciesRun %in% unique(raw_dat$SpeciesRun) else SpeciesRun == input$q_species,
                            if(is.null(input$q_pop_name)) POP_NAME %in% unique(raw_dat$POP_NAME) else POP_NAME == input$q_pop_name,
                            if(is.null(input$q_stream)) StreamName %in% unique(raw_dat$StreamName) else StreamName == input$q_stream)
                   
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
      dataset_name <- datasets %>%
        filter(DatastoreId == isolate(input$datasets)) %>%
        pull(DatastoreName)
      paste0(gsub(' ','_', dataset_name),"_raw_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(cdms_table_data[input[["raw_table_rows_all"]], ], file, row.names = FALSE, na='')
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

  # Redd Summary Grouping Variables
  observeEvent(input$custom_query_menu, {
    if(input$custom_query_menu != 'Redd Summary') {
      output$custom_query_grouping <- renderUI({ NULL }) # dont show grouping inputs if not on Redd summary
      } else {
        output$custom_query_grouping <- renderUI({
          selectInput(inputId = 'custom_grouping', label = 'Choose Grouping Variables:', 
                      choices = c("Species", "Run", "ESU_DPS", "MPG", "POP_NAME", "TRT_POPID", "StreamName", 
                                  "TribToName", "SurveyYear", "TransectName", "Pass"), 
                      selected = NULL, multiple = TRUE)
      })
    }
  })
  
  # Submit Custom Query Request
  observeEvent(input$custom_submit, {
    if(input$custom_query_menu == '-Select Custom Query-') { NULL
      } else {
        if(input$custom_query_menu == 'Redd Summary') {
          tmp_grouping <- input$custom_grouping
          
          if(is.null(tmp_grouping)) {
            RV$cq_data <<- sum_Redds(SGSRedd)
          } else {
            RV$cq_data <<- sum_Redds(SGSRedd, !!!rlang::parse_exprs(tmp_grouping)) %>% ungroup() 
          }
        } else {
      RV$cq_data <<- get(x=custom_query_df[match(input$custom_query_menu, custom_query_df$query_names), 3])
        }
        
        updateSelectInput(session, inputId= 'cq_fields', label= 'Choose Fields in Desired Order:', choices= names(RV$cq_data), selected = NULL)
        
        # Display loaded Custom Query
        output$selected_custom <- renderText({
          paste0(h2('Currently Loaded Custom Query: ', isolate(input$custom_query_menu)))
        })
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
      write.csv(custom_table_data[input[["custom_table_rows_all"]], ], file, row.names = FALSE, na = '')
    },
    contentType = "text/csv"
  )
  
  # FINS data ----
  observeEvent(input$tabs, {
    if(input$tabs == 'tab_fins'){
      
      output$fins_filter <- renderUI({
        if(input$fins_filtertype == 'Facility') {
          list(
            selectInput(inputId = 'fins_facility_filter', label = 'Filter for all data for chosen Facility:', choices = sort(unique(AdultWeirData_clean$facility)),
                        selected = NULL),
            h3("*Filtering by Facility will return all data for the facility (Chosen Facility, All Years).", style='text-align:center;'))
        } else {
          list(
            selectInput(inputId = 'fins_year_filter', label = 'Filter for all data within chosen Year:', choices = sort(unique(AdultWeirData_clean$trap_year)),
                        selected = year(Sys.Date())),
            
            h3("*Filtering by Year will return all data within the specified year (All Facilities, Chosen Year).", style='text-align:center;'))
        }
      })
      
    }
  })
  
  # observeEvent(input$tabs, {
  #   if(input$tabs == 'tab_fins'){
  #     RV$fins_data <<- AdultWeirData
  #     finsRoC <<- 'raw'   # raw or cleaned?
  #
  #     output$selected_fins <- renderText({
  #       paste0(h2('Showing: Raw FINS Data'))
  #     })
  #
  #     updateSelectInput(session, inputId= 'fins_species', label= 'Choose Species:', choices= sort(unique(RV$fins_data$SpeciesRun)),
  #                       selected = NULL)
  #     updateSelectInput(session, inputId= 'fins_location', label= 'Choose Location:', choices= sort(unique(RV$fins_data$Location)),
  #                       selected = NULL)
  #     updateSelectInput(session, inputId= 'fins_fields', label= 'Choose Fields in Desired Order:', choices= names(RV$fins_data),
  #                       selected = NULL)
  #     updateSliderInput(session, inputId = 'fins_year', label = 'Choose Years:', min = min(RV$fins_data$year), max = max(RV$fins_data$year),
  #                       value = c(min(RV$fins_data$year), max(RV$fins_data$year)), step = 1)
  #   }
  # })
  #   # Clear Fields
  # observeEvent(input$fins_clear_fields, {
  #     updateSelectInput(session, inputId= 'fins_species', label= 'Choose Species:', choices= sort(unique(RV$fins_data$SpeciesRun)),
  #                       selected = NULL)
  #     updateSelectInput(session, inputId= 'fins_location', label= 'Choose Location:', choices= sort(unique(RV$fins_data$Location)),
  #                       selected = NULL)
  #     updateSelectInput(session, inputId= 'fins_fields', label= 'Choose Fields in Desired Order:', choices= names(RV$fins_data), selected = NULL)
  #
  #     updateSliderInput(session, inputId = 'fins_year', label = 'Choose Years:', min = min(RV$fins_data$year), max = max(RV$fins_data$year),
  #                       value = c(min(RV$fins_data$year), max(RV$fins_data$year)), step = 1)
  # })
  #
  #   # Update data and display which.
  #   observeEvent(input$fins_raw, {
  #     RV$fins_data <<- AdultWeirData
  #     finsRoC <<- 'raw'
  #
  #     output$selected_fins <- renderText({
  #       paste0(h2('Showing: Raw FINS Data'))
  #     })
  #   })
  #
  #   observeEvent(input$fins_clean, {
  #     RV$fins_data <<- AdultWeirData_clean
  #     finsRoC <<- 'clean'
  #
  #     output$selected_fins <- renderText({
  #       paste0(h2('Showing: Cleaned FINS Data'))
  #     })
  #   })
  #
  #   # FINS Datatable
  # output$fins_table <- DT::renderDataTable({
  #
  #   if(is.null(input$fins_fields)) {
  #     fins_table_data <<- RV$fins_data %>%
  #       filter(year %in% c(min(input$fins_year): max(input$fins_year)))
  #
  #   } else {
  #     fins_table_data <<- RV$fins_data %>%
  #       filter(year %in% c(min(input$fins_year): max(input$fins_year))) %>%
  #       select(input$fins_fields)
  #   }
  #
  #   DT::datatable(fins_table_data, options = list(orderClasses = TRUE), filter = 'top')
  # })
  #
  # FINS Export
  # output$fins_export <- downloadHandler(
  #   filename = function() {
  #     paste(finsRoC,"_fins_data_", Sys.Date(), ".csv", sep='')
  #   },
  #   content = function(file) {
  #     write.csv(fins_table_data, file, row.names = FALSE, na='')
  #   },
  #   contentType = "text/csv"
  # )
  
  # Filter FINS data ----
  # observeEvent(input$fins_filter, {
  #   RV$fins_data <- AdultWeirData_clean %>%
  #     filter(if(input$fins_filtertype == 'Facility') facility == input$fins_filter else trap_year == input$fins_filter)
  # })
  
  # FINS EXPORT (temporary until datatable works)
  output$fins_export <- downloadHandler(
    filename = function() {
      if(input$fins_filtertype == 'Facility') {
        paste("FINS_", gsub(' ','_', input$fins_facility_filter), '_', Sys.Date(), ".csv", sep='')
      } else {
        paste(input$fins_year_filter, '_FINS_all_facilities_', Sys.Date(), ".csv", sep='')
      }
    },
    content = function(file) {
      write.csv(AdultWeirData_clean %>% # apply filter
                  filter(if(input$fins_filtertype == 'Facility') facility == input$fins_facility_filter else trap_year == input$fins_year_filter),
                file, row.names = FALSE, na='')
    },
    contentType = "text/csv"
  )

  # Reports (Tab) ----
  output$pdf_reports <- renderUI({

    report_list <- gsub('_', ' ', gsub('.pdf', '', list.files(path = './pdf/')))

    selectInput('pdf_reports', "Available Reports:", choices = report_list,
                selected = report_list[1])
  })
  
    # Download Reports (already in PDF)
  output$report_export <- downloadHandler(

    filename = function() { 
      paste(gsub(' ', '_', input$pdf_reports), '_', format(Sys.Date(), "%m_%d_%y"), '.pdf', sep='')
    },  
    
    content = function(file){
      # build file path
      report_path <- paste('./pdf/', gsub(' ', '_', input$pdf_reports), '.pdf', sep= '')
      
      file.copy(report_path, file)
    }
  )
  
  # Contact Information Tab -
    # CDMS user info
  output$cdms_users <- DT::renderDataTable({
    DT::datatable(users, options = list(orderClasses = TRUE), filter = 'top')  
  })
  
  
} # close Server
