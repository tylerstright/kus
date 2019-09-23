# Kus UI

# DashboardHeader ----
header <- dashboardHeader(title = div(id = "kustitle", 'Kus', style='float:right;'),  # Title actually shown on browser tab located in dashboardPage()
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
               # menuSubItem('Weir Collections', tabName = 'tab_weir'),
               # menuSubItem('In-Stream Array Abundance', tabName = 'tab_array'),
               menuSubItem('Juvenile Monitoring', tabName = 'tab_juv'),
               menuSubItem('Age Sampling', tabName = 'tab_age')
               ),
      menuItem('Restricted Data Access', tabName = 'tab_rawdata', icon = icon('table'), startExpanded = TRUE,
               menuSubItem('CDMS Datasets', tabName = 'tab_cdms'),
               menuSubItem('Custom Queries', tabName = 'tab_custom'),
               menuSubItem('Reports', tabName = 'tab_reports')
               ),
      br(), br(), br(), br(), br(), # spacers
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
            column(12,
            box(title = 'Spawning Ground Survey Summaries', status='info', width= 5,
                uiOutput(outputId = 'sgs_data_button'),
                uiOutput(outputId = 'sgs_species'),
                uiOutput(outputId = 'sgs_pop_name')
            ),
            box(width = 7, 
              img(src='carcass.png', width = '100%', height='auto') 
                ))
          ),
          fluidRow(
            box(width = 12, plotlyOutput('p_redds'))
          ),
          fluidRow(
            box(width = 4, plotlyOutput('p_females')),
            box(width = 4, plotlyOutput('p_phos')),
            box(width = 4, plotlyOutput('p_psm'))
          ),
          box(width = 12, 
              title = 'Tabular Summary Data',
              fluidRow(column(12, align = "center", downloadButton("sgs_export", label = "Export .CSV File"))),
              div(style = 'overflow-x: scroll;', DT::dataTableOutput('sgs_table'))
          )
  ),
  
  # Weir Collections Summaries Tab ----
  # tabItem(tabName = 'tab_weir',
  #         fluidRow(
  #           box(title = 'Weir Collection Summaries',
  #               helpText('Sorry! This page is currently under contruction.'))
  #         )),
          
  # In-Stream Array Abundance Summaries Tab ----
  # tabItem(tabName = 'tab_array',
  #         fluidRow(
  #           box(title = 'In-Stream Array Summaries',
  #               helpText('Sorry! This page is currently under contruction.'))
  #         )),
          
  # Juvenile Monitoring Summaries Tab ----
  tabItem(tabName = 'tab_juv',
          fluidRow(
            column(12, 
            box(title = 'Juvenile Outmigrant Summary', status='info', width= 5,
                uiOutput(outputId = 'juv_data_button'),
                uiOutput(outputId = 'juv_species'),
                uiOutput(outputId = 'juv_pop_name')
            ),
            box(width = 7, 
                img(src='lostine_rst.jpg', width = '100%', height='auto') 
            ))
          ),
          fluidRow(
            box(width = 12, plotlyOutput('j_abundance'))
                  ),
          fluidRow(
            box(width = 6, plotlyOutput('j_survival')),
            box(width = 6, plotlyOutput('j_equivalents'))
          ),

          box(width = 12, title = 'Tabular Summary Data',
              fluidRow(column(12, align = "center", downloadButton("juv_export", label = "Export .CSV File"))),
              div(style = 'overflow-x: scroll;', DT::dataTableOutput('juv_table'))
              )
          ),
          
  # Age Sampling Tab ----
      tabItem(tabName = 'tab_age',
              fluidRow(
                column(12,
                box(title = 'Age Data Summary', status='info', width= 5,
                    uiOutput(outputId = 'age_data_button'),
                    uiOutput(outputId = 'age_species'),
                    uiOutput(outputId = 'age_pop_name')
                    ),
                box(width = 7, 
                    img(src='scale.png', width = '100%', height='auto') 
                    ))
                ),
              hr(),
              fluidRow(
                column(6,
                       box(width= 12, fluidRow(column(12, plotlyOutput('n_age_total')))),
                       box(width= 12, fluidRow(column(12, plotlyOutput('n_age_ocean')))), 
                       box(width= 12, fluidRow(column(12, plotlyOutput('n_age_stream'))))
                       ),
                column(6,
                       box(width= 12, fluidRow(column(12, plotlyOutput('h_age_total')))),
                       box(width= 12, fluidRow(column(12, plotlyOutput('h_age_ocean')))),
                       box(width= 12, fluidRow(column(12, plotlyOutput('h_age_stream'))))
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
                box(width = 12, title = 'Reports!',
                    h2('There are currently no saved Reports.', style = 'text-align: center;'),
                    h4('This page is itended to be used for automated reports. If you create the same report on a consistent basis (e.g. similar graphs and tables of information), we can work together to automate these reports so they are available at the click of a button with the most current data in CDMS.'),
                    h4('Please contact Tyler Stright (tylers@nezperce.org) with inquiries.')
                    )
              ))
  
    ) #tabItems
  ) #dashboardBody

dashboardPage(title = "Nez Perce Tribe's Kus Web Application", header, sidebar, body, skin= 'blue')
