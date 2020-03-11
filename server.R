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
        
        output$login_link <- renderUI({
          actionLink('greeting', label = paste0('Hello, ', user_info()$Fullname, "!"), style = 'color: white;')
        })
      }
    }
  })
  
  # Home Tab / Leaflet ----
  getPage<-function() {
    return(includeHTML("./www/kus_map.html"))
  }
  output$map<-renderUI({getPage()})
  
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
  
  # create our SGS Reactive Data
  RV <- reactiveValues(sgs_data = NULL)
  
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
             POP_NAME %in% input$sgs_pop_name)
    
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
        layout(title = list(text = '<b>Yearly Total Redd Counts</b>',
                            font = plotly_font),
               yaxis = list(title= 'Total Redds',
                            titlefont = plotly_font),
               xaxis = list(title= 'Spawn Year',
                            titlefont = plotly_font))
    })

    # SGS Carcass - Percent Females
    output$p_females <- renderPlotly({
      
      pf_tmp <- RV$sgs_data %>%
        filter(!is.na(PercentFemales)) %>%
        group_by(POP_NAME)
      
      shiny::validate(
        need(nrow(pf_tmp) > 0, message = '*No data for the current selection.')
      )
      # plot
      pfem_plotly <- plot_ly(data = pf_tmp,
                             x = ~POP_NAME,
                             y = ~PercentFemales,
                             name = ~POP_NAME,
                             type = 'box',
                             hoverinfo = 'y',
                             color = ~POP_NAME,
                             colors = viridis_pal(option="D")(length(unique(pf_tmp$POP_NAME))),
                             showlegend = FALSE
      ) %>%
        layout(title = list(text = '<b>Percent Females</b>',
                            font = plotly_font),
               xaxis= list(title = ''),
               yaxis= list(title= 'Percent Females',
                           titlefont = plotly_font,
                           tickformat = "%",
                           range = c(0,1.05) #,zeroline = FALSE
                           ))
    })
    
    # SGS Carcass - Percent Hatchery Origin Spawners
    output$p_phos <- renderPlotly({
      
      phos_tmp <- RV$sgs_data %>%
        filter(!is.na(pHOS)) %>%
        group_by(POP_NAME)
      
      shiny::validate(
        need(nrow(phos_tmp) > 0, message = '*No data for the current selection.')
      )
      # plot
      phos_plotly <- plot_ly(data = phos_tmp,
                             x = ~POP_NAME,
                             y = ~pHOS,
                             name = ~POP_NAME,
                             type = 'box',
                             hoverinfo = 'y',
                             color = ~POP_NAME,
                             colors = viridis_pal(option="D")(length(unique(phos_tmp$POP_NAME))),
                             showlegend = FALSE
      ) %>%
        layout(title = list(text = '<b>Percent Hatchery Origin Spawners</b>',
                            font = plotly_font),
               xaxis= list(title = ''),
               yaxis= list(title= 'pHOS',
                           titlefont = plotly_font,
                           tickformat = "%",
                           range = c(0,1.05)))
    })
    
    # SGS Carcass - Prespawn Mortalities
    output$p_psm <- renderPlotly({
      
      psm_tmp <- RV$sgs_data %>%
        filter(!is.na(PrespawnMortality)) %>%
        group_by(POP_NAME)
      
      shiny::validate(
        need(nrow(psm_tmp) > 0, message = '*No data for the current selection.')
      )
      # plot
      psm_plotly <- plot_ly(data = psm_tmp,
                             x = ~POP_NAME,
                             y = ~PrespawnMortality,
                             name = ~POP_NAME,
                             type = 'box',
                             hoverinfo = 'y',
                             color = ~POP_NAME,
                             colors = viridis_pal(option="D")(length(unique(psm_tmp$POP_NAME))),
                             showlegend = FALSE
      ) %>%
        layout(title = list(text = '<b>Percent Prespawn Mortality</b>',
                            font = plotly_font),
               xaxis= list(title = ''),
               yaxis= list(title= 'Prespawn Mortality',
                           titlefont = plotly_font,
                           tickformat = "%",
                           range = c(0,1.05)))
    })
    
  })
  
  # SGS Summary Data Table
  
    # Create SGS_Summary Dataset table (reactive/self-updating)
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
        # column(9, actionButton(inputId= 'juv_dataload', label = 'Load Data', icon = icon('hourglass-start'), width = '100%')),
        # column(1, hidden(div(id='juv_spinner', img(src='Fish.gif', style = 'height:30px; float:left;'))))
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
  
  # create our Juvenile Reactive Data
  RV <- reactiveValues(juv_data = NULL)
  
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
        layout(title = list(text = '<b>Natural and Hatchery Origin Smolt Survival to Lower Granite Dam</b>',
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
  
    # Juvenile Summary Data Table 
  
    # Juvenile Summary Dataset table (reactive/self-updating)
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
  
  # Age Samples Tab ----
  
  # UI
  output$age_data_button <- renderUI({
    tagList(
      fluidRow(
        column(12, actionButton(inputId= 'age_dataload', label = 'Click to Load Data', icon = icon('hourglass-start'), width = '100%'))
        # column(9, actionButton(inputId= 'age_dataload', label = 'Load Data', icon = icon('hourglass-start'), width = '100%')),
        # column(1, hidden(div(id='age_spinner', img(src='Fish.gif', style = 'height:30px; float:left;'))))
      ),
      helpText(HTML('<em> *Initial data load may take several minutes.</em>'))
    )
  })
  
  # Load Summarized Age data
  observeEvent(input$age_dataload, {
    shinyjs::disable(id='age_summary_btn')
  
  age_summary_df <<- summariseAGE()[[4]]
  
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
  
  # create our Age Sumamry Reactive Data
  RV <- reactiveValues(age_data = NULL)
  
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
    # Reactive data! This feeds to each graph
    RV$age_data <<- age_summary_df %>%
      filter(SpeciesRun == input$age_species,
             POP_NAME %in% input$age_pop_name,
             POP_NAME != 'NA',
             Origin != 'Unknown')  
    
    # Best Age Graphs
    output$n_age_total <- renderPlotly({
      # NATURAL
      best_tmp <- RV$age_data %>%
        mutate(BestAge = as.character(BestAge)) %>%
        filter(Origin == 'Natural') %>%
        group_by(SpeciesRun, POP_NAME, BestAge, best_weighted_mean, t_colors) %>%
        distinct(BestAge) %>%
        arrange(BestAge)
      
      shiny::validate(
        need(nrow(best_tmp) > 0, message = '*No data for the current selection.')
      )
      
        p_bestage <- plot_ly(data = best_tmp,
                           type = 'bar',
                           orientation = 'h',
                           x = ~best_weighted_mean,
                           y = ~POP_NAME,
                           name = ~BestAge,
                           hovertemplate = paste('%{x}'),
                           # hovertemplate = paste(
                           #   'Age %{name}, %{x}'),
                           marker = list(color = ~t_colors),
                           color = ~BestAge
                           ) %>%
        layout(title = list(text = '<b>Natural Origin Total Ages</b>',
                            font = plotly_font),
               barmode = 'stack',
               xaxis = list(title = 'Percent of Total',
                            titlefont = plotly_font,
                            tickformat = "%",
                            range = c(0,1.05)),
               yaxis = list(title = ''))  

    }) 
    
    output$h_age_total <- renderPlotly({
      # HATCHERY
      best_tmp <- RV$age_data %>%
        mutate(BestAge = as.character(BestAge)) %>%
        filter(Origin == 'Hatchery') %>%
        group_by(SpeciesRun, POP_NAME, BestAge, best_weighted_mean, t_colors) %>%
        distinct(BestAge) %>%
        arrange(BestAge)
      
      shiny::validate(
        need(nrow(best_tmp) > 0, message = '*No data for the current selection.')
      )
      
      p_bestage <- plot_ly(data = best_tmp,
                           type = 'bar',
                           orientation = 'h',
                           x = ~best_weighted_mean,
                           y = ~POP_NAME,
                           name = ~BestAge,
                           hovertemplate = paste('%{x}'),
                           marker = list(color = ~t_colors)
                           ) %>%
        layout(title = list(text = '<b>Hatchery Origin Total Ages</b>',
                            font = plotly_font),
               barmode = 'stack',
               xaxis = list(title = 'Percent of Total',
                            titlefont = plotly_font,
                            tickformat = "%",
                            range = c(0,1.05)),
               yaxis = list(title = ''))  

    })
    
    # Ocean Age Graphs
    output$n_age_ocean <- renderPlotly({
      # NATURAL
      ocean_tmp <- RV$age_data %>% 
        mutate(OceanAge = as.factor(OceanAge)) %>%
        filter(Origin == 'Natural') %>%
        group_by(SpeciesRun, POP_NAME, OceanAge, ocean_weighted_mean, o_colors) %>%
        distinct(OceanAge) %>%
        arrange(OceanAge)
      
      shiny::validate(
        need(nrow(ocean_tmp) > 0, message = '*No data for the current selection.')
      )

        p_oceanage <- plot_ly(data = ocean_tmp,
                             type = 'bar',
                             orientation = 'h',
                             x = ~ocean_weighted_mean,
                             y = ~POP_NAME,
                             name = ~OceanAge,
                             hovertemplate = paste('%{x}'),
                             marker = list(color = ~o_colors)
                             ) %>%
          layout(title = list(text = '<b>Natural Origin Ocean Ages</b>',
                              font = plotly_font),
                 barmode = 'stack',
                 xaxis = list(title = 'Percent of Total',
                              titlefont = plotly_font,
                              tickformat = "%",
                              range = c(0,1.05)),
                 yaxis = list(title = '')) 
    })  
    
    output$h_age_ocean <- renderPlotly({
      # HATCHERY
      ocean_tmp <- RV$age_data %>% 
        mutate(OceanAge = as.factor(OceanAge)) %>%
        filter(Origin == 'Hatchery') %>%
        group_by(SpeciesRun, POP_NAME, OceanAge, ocean_weighted_mean, o_colors) %>%
        distinct(OceanAge) %>%
        arrange(OceanAge)
      
      shiny::validate(
        need(nrow(ocean_tmp) > 0, message = '*No data for the current selection.')
      )
      
      p_oceanage <- plot_ly(data = ocean_tmp,
                            type = 'bar',
                            orientation = 'h',
                            x = ~ocean_weighted_mean,
                            y = ~POP_NAME,
                            name = ~OceanAge,
                            hovertemplate = paste('%{x}'),
                            marker = list(color = ~o_colors)
                            ) %>%
        layout(title = list(text = '<b>Hatchery Origin Ocean Ages</b>',
                            font = plotly_font),
               barmode = 'stack',
               xaxis = list(title = 'Percent of Total',
                            titlefont = plotly_font,
                            tickformat = "%",
                            range = c(0,1.05)),
               yaxis = list(title = '')) 
    }) 
    
    # Stream Age Graphs
    output$n_age_stream <- renderPlotly({
      # NATURAL
      stream_tmp <- RV$age_data %>% 
        mutate(StreamAge = as.factor(StreamAge)) %>%
        filter(Origin == 'Natural') %>%
        group_by(SpeciesRun, POP_NAME, StreamAge, stream_weighted_mean, s_colors) %>%
        distinct(StreamAge) %>%
        arrange(StreamAge)
      
      shiny::validate(
        need(nrow(stream_tmp) > 0, message = '*No data for the current selection.')
      )

      p_streamage <- plot_ly(data = stream_tmp,
                           type = 'bar',
                           orientation = 'h',
                           x = ~stream_weighted_mean,
                           y = ~POP_NAME,
                           name = ~StreamAge,
                           hovertemplate = paste('%{x}'),
                           marker = list(color = ~s_colors)
                           ) %>%
        layout(title = list(text = '<b>Natural Origin Stream Ages</b>',
                            font = plotly_font),
               barmode = 'stack',
               xaxis = list(title = 'Percent of Total',
                            titlefont = plotly_font,
                            tickformat = "%",
                            range = c(0,1.05)),
               yaxis = list(title = '')) 
    })
    
    output$h_age_stream <- renderPlotly({
      # HATCHERY
      stream_tmp <- RV$age_data %>% 
        mutate(StreamAge = as.factor(StreamAge)) %>%
        filter(Origin == 'Hatchery') %>%
        group_by(SpeciesRun, POP_NAME, StreamAge, stream_weighted_mean, s_colors) %>%
        distinct(StreamAge) %>%
        arrange(StreamAge)
      
      shiny::validate(
        need(nrow(stream_tmp) > 0, message = '*No data for the current selection.')
      )
      
      p_streamage <- plot_ly(data = stream_tmp,
                             type = 'bar',
                             orientation = 'h',
                             x = ~stream_weighted_mean,
                             y = ~POP_NAME,
                             name = ~StreamAge,
                             hovertemplate = paste('%{x}'),
                             marker = list(color = ~s_colors)
                             ) %>%
        layout(title = list(text = '<b>Hatchery Origin Stream Ages</b>',
                            font = plotly_font),
               barmode = 'stack',
               xaxis = list(title = 'Percent of Total',
                            titlefont = plotly_font,
                            tickformat = "%",
                            range = c(0,1.05)),
               yaxis = list(title = '')) 
    })
    
  })
  
  # Restricted Data Access ====================================================
  # CDMS DATASETS ----
  # Create ***REACTIVE VALUES*** (RV$) for dynamic data and Inputs
  RV <- reactiveValues(query_data = NULL)
  
  # Clear Field Values Button ----
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
      filter(!DatastoreId %in% c(81:84, 87:91, 93, 98)) %>% 
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

    if(input$datasets %in% c(78, 79)) { # =c("SGS Redd Data", "SGS Carcass Data")
      raw_dat <<- getDatasetView(datastoreID = input$datasets, cdms_host = cdms_host) 
      # Prepare Adult Data (i.e. Survey Date)
      raw_dat <<- raw_dat %>%
        mutate(SpeciesRun = paste(Run, Species),
               SurveyDate = as_date(SurveyDate),
               Year = year(SurveyDate)) %>% 
        select(-contains('Id'))
    } else {
      if(input$datasets %in% c(85, 86)) { # = c('NPT RST Abundance Estimates', 'NPT Juvenile Survival Estimates')
        raw_dat <<- getDatasetView(datastoreID = input$datasets, cdms_host = cdms_host)
      # Prepare Juvenile Data (i.e. Migratory Year)
      raw_dat <<- raw_dat %>%
        mutate(SpeciesRun = paste(Run, Species),
               Year = MigratoryYear) %>% 
        select(-contains('Id'))
      } else {
        raw_dat <<- getDatasetView(datastoreID = input$datasets, cdms_host = cdms_host)
        # Prepare Age Data (i.e. Collection Date)
        raw_dat <<- raw_dat %>%
          mutate(SpeciesRun = paste(Run, Species),
                 CollectionDate = as_date(CollectionDate),
                 Year = year(CollectionDate)) %>% 
          select(-contains('Id'))
      }

    }
  
      RV$query_data <<- raw_dat  # Populate our dynamic dataframe.
      
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
