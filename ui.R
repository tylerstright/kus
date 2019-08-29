# Kus UI

# DashboardHeader ----
header <- dashboardHeader(title = div(id = "kustitle", 'Kus', style='float:right;'),
                          tags$li(img(src='NPTlogos2.png', title = NULL, draggable = FALSE, height = '40px'), 
                                  class = 'dropdown', style = 'position: fixed; left:40px; padding-top:6px'),
                          tags$li(tags$a("DFRM Home", href= 'http://www.nptfisheries.org', target ='_blank', class='navlinks'),
                                  class = 'dropdown',  style = "float:left;"),
                          tags$li(tags$a("PITPH Web App", href = "https://nptfisheries.shinyapps.io/pitph2/", target = '_blank', class='navlinks'),
                                  class = 'dropdown'),
                          tags$li(tags$a("PITtrackR Web App", href = "https://nptfisheries.shinyapps.io/PITtrackR/", target = '_blank', class='navlinks'),
                                  class = 'dropdown'),
                          # Login/Logout Link
                          tags$li(tags$a(uiOutput('login_logout')), class = 'dropdown', style= 'position:absolute; left:42px')
                          )


# Dashboard Sidebar ----
sidebar <- dashboardSidebar(
    useShinyjs(), # Activate ShinyJS
    sidebarMenu(
      menuItem('Kus Home', tabName = 'tab_home', icon = icon("home")),
      menuItem('Data Summaries', tabName = 'tab_productivity', icon = icon("chart-area"), startExpanded = TRUE,
               menuSubItem('Spawning Ground Surveys', tabName = 'tab_sgs'),
               menuSubItem('Weir Collections', tabName = 'tab_weir'),
               menuSubItem('In-Stream Array Abundance', tabName = 'tab_array'),
               menuSubItem('Juvenile Monitoring', tabName = 'tab_juv'),
               menuSubItem('Age Sampling', tabName = 'tab_age')
               ),
      menuItem('Restricted Data Access', tabName = 'tab_rawdata', icon = icon('table'), startExpanded = TRUE,
               menuSubItem('CDMS Datasets', tabName = 'tab_cdms'),
               menuSubItem('Custom Queries', tabName = 'tab_custom'),
               menuSubItem('Reports', tabName = 'tab_reports')
               ),
      br(), br(), br(), br(), br(), # spacers.
      img(src = 'DFRM.png', title = NULL, draggable = FALSE, width = '100%', style = 'padding-left:10px;') # DFRM Logo
    )
  )

# Dashboard Body ----
body <- dashboardBody(
  includeCSS('./www/styles.css'),
  # img(src='kusbg.jpg', class = 'kusbg', draggable = FALSE), # background image
    tabItems(
  # KusHome Tab ----
      tabItem(tabName = 'tab_home',
              fluidRow(
                column(5,
                box(status = 'info', width=12, background = 'aqua', # ?validStatuses ?validColors
                            p('The Kus web application is intended to provide near real-time data summaries and
                              visualizations to Nez Perce Tribal members and general public. This tool supports Department 
                              of Fisheries Resources Management staff and Snake Basin fisheries management decisions.')
                            , style = 'color:black; font-size:1.23vw;'),
                       box(status = 'info', width = 12, 
                           img(src='Steelhead.jpg', width = '100%', height='auto')),
                       box(status = 'info', width = 12, 
                           img(src='jcweir.jpg', width = '100%', height='auto'))
                ),
                column(7, 
                box(status = 'info', width = 12, 
                  img(src='jcrst.jpg', width = '100%', height = 'auto')),
                box(status = 'info', width = 12, 
                    img(src='chinook.jpg', width = '100%', height = 'auto'))
                )
              )
            ),

  # Spawning Ground Survey Summaries Tab ----
  tabItem(tabName = 'tab_sgs',
          fluidRow(
            box(title = 'Spawning Ground Survey Summaries', status='info', width= 5,
                fluidRow(
                  column(9, actionButton(inputId= 'sgs_dataload', label = 'Load Data', icon = icon('hourglass-start'), width = '100%')),
                  column(1, hidden(div(id='sgs_spinner', img(src='Fish.gif', style = 'height:30px; '))))
                         ),
                helpText(HTML('<em> *Initial data load may take several minutes.</em>')),
                uiOutput(outputId = 'sgs_species'),
                uiOutput(outputId = 'sgs_pop_name')
            ),
            box(width = 7, 
              img(src='lostine_rst.jpg', width = '100%', height='auto') 
                )
          ),
          fluidRow(
            box(title = "Total Redds by Year", width = 12,
                plotlyOutput('p_redds'))
          ),
          fluidRow(
            box(title = "Total Carcasses by Year", width = 12,
                plotlyOutput('p_carcass'))
          )
  ),
  
  # Weir Collections Summaries Tab ----
  tabItem(tabName = 'tab_weir',
          fluidRow(
            box(title = 'Weir Collection Summaries',
                helpText('Sorry! This page is currently under contruction.'))
          )),
          
  # In-Stream Array Abundance Summaries Tab ----
  tabItem(tabName = 'tab_array',
          fluidRow(
            box(title = 'In-Stream Array Summaries',
                helpText('Sorry! This page is currently under contruction.'))
          )),
          
  # Juvenile Monitoring Summaries Tab ----
  tabItem(tabName = 'tab_juv',
          fluidRow(
            box(title = 'Juvenile Summaries', status='info', width= 5,
                
                fluidRow(
                  column(9, actionButton(inputId= 'juv_dataload', label = 'Load Data', icon = icon('hourglass-start'), width = '100%')),
                  column(1, hidden(div(id='juv_spinner', img(src='Fish.gif', style = 'height:30px; float:left;'))))
                ),
                helpText(HTML('<em> *Initial data load may take several minutes.</em>')),
                uiOutput(outputId = 'juv_species'),
                uiOutput(outputId = 'juv_pop_name')
            ),
            box(width = 7, 
                img(src='lostine_rst.jpg', width = '100%', height='auto') 
            )
          ),
          fluidRow(
            box(title = "Natural Juvenile Abundance Estimates", width = 12, plotlyOutput('j_abundance'))
                  ),
          fluidRow(
            box(title = "Natural Juvenile Survival Estimates to Lower Granite Dam", width = 12, plotlyOutput('j_survival'))
                  ),
          fluidRow(
            box(title = "Natural Juvenile Equivalents at Lower Granite Dam", width = 12, plotlyOutput('j_equivalents'))
                  )
          ),
          
  # Age Sampling Tab ----
      tabItem(tabName = 'tab_age',
              box(width = 12, 
                  fluidRow(
                    column(2, offset = 5, actionButton('age_summary_btn', label = 'Populate Age Summaries', icon = icon('table'))
                           ),
                    column(5, hidden(div(id='age_spinner',img(src='Fish.gif', style = 'height:30px'))))
                          ),
                  helpText(HTML('<em> *Data load may take several minutes.</em>'), style = 'text-align:center;'),
                  hr(), 
                  fluidRow(column(12, plotlyOutput('age_total')))
                  ),
              fluidRow(
              box(width = 6,
                  fluidRow(column(12, plotlyOutput('age_stream')))
                  ),
              box(width = 6, 
                  fluidRow(column(12, plotlyOutput('age_ocean')))
                  ) 
                )
              ), # tab
  # Restricted Data Access Tab ----
    # CDMS Datasets ----
      tabItem(tabName = 'tab_cdms',
              box(width = 12, 
              fluidRow(column(6, uiOutput("raw_dataset_menu"),
                              # helpText(HTML("<em> Select the desired dataset and click 'Load Data'. </em>"), style = 'text-align:center;'),
                              fluidRow(
                                column(8, offset = 2, actionButton("raw_submit", label = "Load Data", icon = icon('hourglass-start'), width = '100%')),
                                column(2, hidden(div(id='datasets_spinner',img(src='Fish.gif', style = 'height:30px'))))
                                      ),
                              br(),
                              selectInput(inputId = 'q_fields', label = 'Choose Fields in Desired Order:', choices = NULL, selectize = TRUE, multiple = TRUE),
                              sliderInput(inputId= 'q_year', label= '*Choose Years:', min = 0, max = 100, value=  c(0,100), sep= '', step = 1),
                              helpText(HTML('<em>* Year is "Spawn Year" for adult datasets and "Migratory Year" for juvenile datasets.</em>'), style = 'text-align:center;')
                              ),
                       column(6, 
                              selectInput(inputId= 'q_species', label= 'Choose Species:', choices= NULL, selected = NULL, multiple = TRUE),
                              selectInput(inputId= 'q_pop_name', label= 'Choose Population:', choices= NULL, selected = NULL, multiple = TRUE),
                              selectInput(inputId= 'q_stream', label= 'Choose Stream:', choices= NULL, selected = NULL, multiple = TRUE),
                              # selectInput(inputId= 'q_transect', label= 'Choose Transect:', choices= NULL, selected = NULL, multiple = TRUE),
                              br(),
                              fluidRow(
                                column(8, offset = 2, actionButton('clear_fields', HTML('<strong> Clear Field Values </strong>'), width = '100%'))
                                )
                              )
              ),
              hr(),
              fluidRow(column(12, align = "center", downloadButton("raw_export", label = "Export .CSV File"))),
              div(style = 'overflow-x: scroll;', DT::dataTableOutput('raw_table'))
              )
      ),
    # Custom Queries ----
      tabItem(tabName = 'tab_custom',
                box(width = 12,
                    helpText(h3('This page is intended to meet the needs of Projects and Biologists to produce desired data views or 
                             summaries. Please contact the Data Management team with requests or inquiries!'), style = 'text-align:center;'),
                    hr(),
                    fluidRow(column(6, 
                                    selectInput('custom_query_menu', label = NULL, choices = query_names, selected = '-Select Custom Query-'),
                                    uiOutput('query_description', style = 'text-align:center;'),
                                    br(),
                                    fluidRow(
                                      column(8, offset=2, actionButton("custom_submit", label = "Submit Query", icon = icon('hourglass-start'), width = '100%')),
                                      column(2, hidden(div(id='query_spinner',img(src='Fish.gif', style = 'height:30px'))))
                                            )
                                    ),
                             column(6, 
                                    selectInput(inputId = 'cq_fields', label = NULL, choices = NULL, selectize = TRUE, multiple = TRUE),
                                    helpText(HTML('<em>Select desired fields in preferred order.</em>'), style='text-align:center;')
                                    )
                             )
                ),
              box(width = 12, 
                  fluidRow(column(12, align = "center", downloadButton("custom_export", label = "Export .CSV File"))),
                  div(style = 'overflow-x: scroll;', DT::dataTableOutput('custom_table'))
              )
        ),
    # Reports ----
      tabItem(tabName = 'tab_reports',
              fluidRow(
                box(title = 'Reports!',
                    helpText('Sorry! This page is currently under construction. Come back later!'))
              ))
  
    ) #tabItems
  ) #dashboardBody

dashboardPage(header, sidebar, body, skin= 'blue')
