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
               menuSubItem('Weir Collections', tabName = 'tab_weir'),
               menuSubItem('In-Stream Array Abundance', tabName = 'tab_array'),
               menuSubItem('Juvenile Monitoring', tabName = 'tab_juv')
               ),
      menuItem('Restricted Data Access', tabName = 'tab_rawdata', icon = icon('table'), startExpanded = TRUE,
               menuSubItem('CDMS Datasets', tabName = 'tab_cdms'),
               menuSubItem('Custom Queries', tabName = 'tab_queries'),
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
                selectInput(inputId= 'sgs_species', label= 'Choose Species:', choices= species_list, selectize= FALSE, 
                            selected = 'Spring/Summer Chinook Salmon', multiple = FALSE),
                selectInput(inputId= 'sgs_pop_name', label= 'Choose Population:', choices= population_list, selectize= FALSE, multiple = FALSE,
                            selected= 'East Fork South Fork Salmon River'),
                fluidRow(
                  column(9, actionButton(inputId = 'sgs_submit', label = 'Populate Summaries', icon = icon('table'), style = 'float:right;')),
                  column(1, hidden(div(id='sgs_spinner', img(src='Fish.gif', style = 'height:30px;'))))
                ),
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
  tabItem(tabName = 'tab_weir',
          fluidRow(
            box(title = 'Weir Collection Summaries',
                helpText('Sorry! This page is currently under contruction. Our Data Nerd is working on it.'))
          )),
          
  # In-Stream Array Abundance Summaries Tab ----
  tabItem(tabName = 'tab_array',
          fluidRow(
            box(title = 'Weir Collection Summaries',
                helpText('Sorry! This page is currently under contruction. Our Data Nerd is working on it.'))
          )),
          
  # Juvenile Monitoring Summaries Tab ----
  tabItem(tabName = 'tab_juv',
          fluidRow(
            box(title = 'Juvenile Summaries', status='info', width= 5,
                selectInput(inputId= 'juv_species', label= 'Choose Species:', choices= species_list, selectize= FALSE, 
                            selected = 'Spring/Summer Chinook Salmon', multiple = FALSE),
                selectInput(inputId= 'juv_pop_name', label= 'Choose Population:', choices= population_list, selectize= FALSE, 
                            selected = 'East Fork South Fork Salmon River', multiple = FALSE),
                fluidRow(
                  column(9, actionButton(inputId = 'juv_submit', label = 'Populate Summaries', icon = icon('table'), style = 'float:right;')),
                  column(1, hidden(div(id='juv_spinner', img(src='Fish.gif', style = 'height:30px;'))))
                ),
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
    # CDMS Datasets ----
      tabItem(tabName = 'tab_cdms',
              box(width = 12, 
              fluidRow(column(4, uiOutput("raw_dataset_menu"),
                              helpText(HTML("<em> Select the desired dataset and click submit query. </em>"), style = 'text-align:center;'),
                              fluidRow(
                                column(8, offset = 2, actionButton("raw_submit", label = "Submit Query", icon = icon('hourglass-start'), width = '100%')),
                                column(2, hidden(div(id='datasets_spinner',img(src='Fish.gif', style = 'height:30px'))))
                                      ),
                              uiOutput(outputId = 'q_species'),
                              uiOutput(outputId = 'q_pop_name')
                              ),
                       column(4, uiOutput(outputId = 'q_stream'),
                                 uiOutput(outputId = 'q_locationlabel'),
                                 uiOutput(outputId = 'q_year')
                              ), 
                       column(4, uiOutput('dataset_fields'),
                                 uiOutput('datasetfield_submit') 
                              )
              ),
              hr(),
              fluidRow(column(12, align = "center", hidden(downloadButton("raw_export", label = "Export .CSV File")))),
              div(style = 'overflow-x: scroll;', DT::dataTableOutput('raw_table'))
              )
      ),
    # Custom Queries ----
      tabItem(tabName = 'tab_queries',
              fluidRow(
                box(width = 12,
                    helpText('This page is a placeholder for custom queries.  Projects and Biologists need to request desired data views or 
                             summaries and these will be created to meet those needs. Contact the Data Management team with requests or inquiries.')
                )
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
