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
                          tags$li(tags$a(uiOutput('login_logout')), class = 'dropdown', 
                                  style= 'position:absolute; left:42px')
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
               menuSubItem('Juvenile Monitoring', tabName = 'tab_juv')
               ),
      menuItem('Restricted Data Access', tabName = 'tab_rawdata', icon = icon('table'), startExpanded = TRUE,
               menuSubItem('CDMS Datasets', tabName = 'tab_cdms'),
               menuSubItem('Custom Queries', tabName = 'tab_queries')#,
               # menuSubItem('Reports', tabName = 'tab_reports')
               ),
      br(), br(), br(), br(), br(),
      img(src = 'DFRM.png', title = NULL, draggable = FALSE, width = '100%', style = 'padding-left:10px;')
    )
  )

# Dashboard Body ----
body <- dashboardBody(
  includeCSS('./www/styles.css'),
  img(src='kusbg.jpg', class = 'kusbg', draggable = FALSE), # background image
    tabItems(
  # KusHome Tab ----
      tabItem(tabName = 'tab_home',
              fluidRow(
                column(5,
                box(status = 'info', width=12, background = 'aqua', # ?validStatuses ?validColors
                            p(HTML('<em> The Kus web application is intended to provide near real-time data summaries and
                              visualizations to Nez Perce Tribal members and general public. This tool supports Department 
                              of Fisheries Resources Management staff and Snake Basin fisheries management decisions. </em>')
                            , style = 'color:black; font-size:1.23vw;')),
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
              ),
              fluidRow(
                column(12,
                       helpText(HTML("<em> Please contact tylers@nezperce.org with any questions, comments, requests, or recommendations for the Nez Perce Tribe's Kus web application. </em>"),  
                                width = '100%', style='color:black; text-align:center;')
                      )
                )
            ),

  # Spawning Ground Survey Summaries Tab ----
  tabItem(tabName = 'tab_sgs',
          fluidRow(
            box(title = 'Spawning Ground Survey Summaries', status='info', width= 5,
                uiOutput(outputId = 'sgs_species'),
                uiOutput(outputId = 'sgs_pop_name'),
                uiOutput(outputId = 'sgs_btn_summary', style = 'text-align:center;'),
                helpText(HTML('<em> *Not all Species/Population combinations will return data.</em>')),
                helpText(HTML('<em> *Data load may take several moments.</em>'))
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
          )#,
          # fluidRow(
          #   box(title = "Escapement by Year", width = 6),
          #   box(title = "Adult Age Composition", width = 6)
          # )
  ),
  
  # Weir Collections Summaries Tab ----
  # tabItem(tabName = 'tab_weir),
          
  # In-Stream Array Abundance Summaries Tab ----
  # tabItem(tabName = 'tab_array'),
          
  # Juvenile Monitoring Summaries Tab ----
  tabItem(tabName = 'tab_juv',
          fluidRow(
            box(title = 'Juvenile Summaries', status='info', width= 5,
                uiOutput(outputId = 'juv_species'),
                uiOutput(outputId = 'juv_pop_name'),
                uiOutput(outputId = 'juv_btn_summary', style = 'text-align:center;'),
                helpText(HTML('<em> *Not all Species/Population combinations will return data.</em>')),
                helpText(HTML('<em> *Data load may take several moments.</em>'))
            ),
            box(width = 7, 
                img(src='lostine_rst.jpg', width = '100%', height='auto') 
            )
          ),
          fluidRow(
            box(title = "Natural Juvenile Abundace Estimates", width = 12, plotlyOutput('j_abundance'))
                  ),
          fluidRow(
            box(title = "Natural Juvenile Survival Estimates to Lower Granite Dam", width = 12, plotlyOutput('j_survival'))
                  )#,
          # fluidRow(
          #   box(title = "Smolt Equivalents", width = 6),
          #   box(title = "Hatchery Releases", width = 6)
          # )
          ),
          
  # Restricted Data Access Tab ----
    # CDMS Datasets
      tabItem(tabName = 'tab_cdms',
              box(width = 12, 
              fluidRow(column(3, uiOutput("raw_dataset_menu"))
              ),
              fluidRow(column(6, helpText("Select the desired dataset then the project, stream and locations of interest and click submit query.")),
                       column(2, offset = 2, align = "center",
                              actionButton("raw_submit", label = "Submit Query", class = "mybutton")),
                       column(2, align = "center",
                              downloadButton("raw_export", label = "Export .CSV File", class = "mybutton")) # this class isn't included in CSS file.
                       ),
              hr(),
              div(style = 'overflow-x: scroll;', withSpinner(DT::dataTableOutput('raw_table')))
              )
      ),
    # Custom Queries
      tabItem(tabName = 'tab_queries',
              fluidRow(
                box(width = 12, height = 'auto',
                    fluidRow(
                      column(4,
                       uiOutput(outputId = 'q_datasets'),
                       uiOutput(outputId = 'btn_q_datasets', style = 'text-align:center;'),
                       helpText(HTML('<em> * Note: This step may take several minutes. </em>'), style = 'text-align:center;'),
                       helpText(HTML('<em> ** Clicking this button will always result in a wait time for data retrieval.</em>'), style = 'text-align:center;'),
                       hr(),
                       uiOutput(outputId = 'q_species')
                       ),
                       column(4,
                       uiOutput(outputId = 'q_pop_name'),
                       uiOutput(outputId = 'q_stream')
                       ),
                       column(4,
                       uiOutput(outputId = 'q_locationlabel'),
                       uiOutput(outputId = 'q_year'),
                       uiOutput(outputId = 'btn_q_summary', style = 'text-align:center;')
                       )
                    )
                )
              ),
              fluidRow(
                box(width = 12,
                       downloadButton("query_export", label = "Export .CSV File", style = 'text-align:center;', class = "mybutton"),
                       hr(),
                       div(style = 'overflow-x: scroll;', DT::dataTableOutput('query_table', width= '50%'))
                )
              )
        ),
    # Reports
      tabItem(tabName = 'tab_reports')
  
    ) #tabItems
  ) #dashboardBody

dashboardPage(header, sidebar, body, skin= 'blue')
