# Kus UI

# DashboardHeader ----
header <- dashboardHeader(title = div(id = "kustitle", 'Kus', style='float:right;'),  # Title shown on browser tab located in dashboardPage()
                          tags$li(img(src='NPTlogos2.png', title = NULL, draggable = FALSE, height = '40px'), 
                                  class = 'dropdown', style = 'position: fixed; left:40px; padding-top:6px'),
                          tags$li(tags$a("PITPH Web App", href = "https://nptfisheries.shinyapps.io/pitph2/", target = '_blank', class='navlinks'),
                                  class = 'dropdown'),
                          tags$li(tags$a("PITtrackR Web App", href = "https://nptfisheries.shinyapps.io/PITtrackR/", target = '_blank', class='navlinks'),
                                  class = 'dropdown'),
                          # Login/Logout Link
                          tags$li(tags$a(uiOutput('login_link')), class = 'dropdown', style= 'position:absolute; left:42px')
                          )

# Dashboard Sidebar ----
sidebar <- dashboardSidebar(
    useShinyjs(), # Activate ShinyJS
    tags$script(src='javascript.js'), # include Javascript file (for custom spinner functionality)
    sidebarMenu(id = 'tabs',
      menuItem('Kus Home', tabName = 'tab_home', icon = icon("home")),
      menuItem('DFRM Divisions', tabName = 'tab_divisions', startExpanded = TRUE,
               menuSubItem('Administration', tabName = 'tab_administration'),
               menuSubItem('Harvest', tabName = 'tab_harvest'),
               menuSubItem('Production', tabName = 'tab_production'),
               menuSubItem('Research', tabName = 'tab_research'),
               menuSubItem('Watershed', tabName = 'tab_watershed'),
               menuSubItem('Conservation Enforcement', tabName = 'tab_conservation')
               ),
      menuItem('Data Summaries', tabName = 'tab_productivity', icon = icon("chart-area"), startExpanded = TRUE,
               menuSubItem('Spawning Ground Surveys', tabName = 'tab_sgs'),
               menuSubItem('Weir Collections', tabName = 'tab_weir'),
               # menuSubItem('In-Stream Array Abundance', tabName = 'tab_array'),
               menuSubItem('Juvenile Monitoring', tabName = 'tab_juv'),
               menuSubItem('Age Sampling', tabName = 'tab_age')
               ),
      menuItem('Documents', tabName = 'tab_documents', icon = icon("file")),
      menuItem('Restricted Data Access', tabName = 'tab_rawdata', icon = icon('table'), startExpanded = TRUE,
               menuItemOutput('rd_cdms'),
               menuItemOutput('rd_customquery'),
               menuItemOutput('rd_fins'),
               menuItemOutput('rd_reports')
               ),
      menuItem('Contact Information', tabName = 'tab_contacts'),
      br(), br(), br(), br(), 
      helpText(HTML(paste('Data Version: ', deploy_time)), style = 'position:absolute; vertical-align:bottom;
               color:white; left:12px; bottom:5px;'),
      div(class = 'busy',
          img(src="kus_spinner.gif", height= 'auto', width = '100%') # Ty's B.A. custom Spinner
      )
    )
  )

# Dashboard Body ----
body <- dashboardBody(
  includeCSS('./www/styles.css'),
    tabItems(
  # KusHome Tab ----
  tabItem(tabName = 'tab_home',
          br(),
          fluidRow(
            column(10, offset = 0,
                   h1('Department of Fisheries Resources Management', style = 'text-align:center;'),
                   br(),
                   column(10, offset = 1,
                          box(status = 'info', width=12, background = 'aqua',
                              p("The Nez Perce Department of Fisheries Resources Management will protect and restore aquatic resources and habitats. Our mission will be accomplished consistent with the Nimmipuu way of life and beliefs, which have the utmost respect for the Creator, for all species, and for the past, present and future generations to come. Our mission will be consistent with the reserved rights stated within the Nez Perce Tribe's 1855 Treaty."),
                          )),
                   br()
            ),
            column(2,
                   tags$a(
                       href = 'https://npt-cdms.nezperce.org/services/uploads/P/11066/DFRM%20Management%20Plan.pdf',
                       tags$img(src='DFRM Management Plan Cover.jpg',
                                title = 'Click to view the DFRM Management Plan',
                                width = 'auto',
                                height = '180')
                     )
            )
            
          ),
          
          box(width = NULL, solidHeader = TRUE, status = 'primary',
              title = 'Lower Granite Window Counts',
              fluidRow(
                column(3,
                       valueBoxOutput("windowChinook", width = NULL)
                ),
                column(3,
                       valueBoxOutput("windowSteelhead", width = NULL)
                ),
                column(3,
                       valueBoxOutput("windowCoho", width = NULL)
                ),
                column(3,
                       valueBoxOutput("windowSockeye", width = NULL)
                )
              ),
              fluidRow(
                column(12, plotlyOutput('window_plot'))
              )
          )
  ),
  # Administration Tab ----
  tabItem('tab_administration', br(),
          h2('Administration Information', style = 'text-align:center; color:white; font-family: Arial White;'),
          fluidRow(
            column(10, offset = 1,
                   box(status = 'info', width=12, background = 'aqua',
                       p("Administration Description Here!")
                   )),
            column(6, offset = 3,
                   box(title = NULL, width = 12, status = 'info', 
                       h3("Division Director"),
                       p('Dave Johnson  (Email: davej@nezperce.org)'),
                       br(),
                       h3("Administrative Specialist"),
                       p('Michelle Wilson  (Email: michelw@nezperce.org)')
                   ))
          )
  ),
  
  # Harvest Tab ----
  tabItem('tab_harvest', br(),
          h2('Harvest Information', style = 'text-align:center; color:white; font-family: Arial White;'),
          fluidRow(
            column(10, offset = 1,
                   box(status = 'info', width=12, background = 'aqua',
                       p("Harvest Description Here!")
                   )),
            column(6, offset = 3,
                   box(title = NULL, width = 12, status = 'info', 
                       h3("Division Director"),
                       p('Joe Oatman  (Email: joeo@nezperce.org)'),
                       # br(),
                       # h3("Administrative Specialist"),
                       # p('  (Email: @nezperce.org)')
                   ))
          )
  ),
  
  # Production Tab ----
  tabItem('tab_production', br(),
          h2('Production Information', style = 'text-align:center; color:white; font-family: Arial White;'),
          fluidRow(
            column(10, offset = 1,
                   box(status = 'info', width=12, background = 'aqua',
                       p("Production Description Here!")
                   )),
            column(6, offset = 3,
                   box(title = NULL, width = 12, status = 'info', 
                       h3("Division Director"),
                       p('Becky Johnson  (Email: beckyj@nezperce.org)'),
                       br(),
                       h3("Administrative Specialist"),
                       p('Tish Whitman  (Email: tishw@nezperce.org)')
                   ))
          ),
          box(title = 'Project Information', solidHeader = TRUE, width = 12, status = 'info', 
              uiOutput(outputId = 'production_select'),
              h3("Description"),
              uiOutput('production_description'), br(),br(),
              h3('Objectives'),
              uiOutput('production_objectives'), br(),br(),
              h3('Project Leader'),
              uiOutput('production_PL'), br(),br(),
              h3('Staff'),
              uiOutput('production_staff')
          )
  ),
  
  # Research Tab ----
  tabItem('tab_research', br(),
          h2('Research Information', style = 'text-align:center; color:white; font-family: Arial White;'),
          fluidRow(
            column(10, offset = 1,
                   box(status = 'info', width=12, background = 'aqua',
                       p("Research Description Here!")
                   )),
            column(6, offset = 3,
                   box(title = NULL, width = 12, status = 'info', 
                       h3("Division Director"),
                       p('Jason Vogal  (Email: jasonv@nezperce.org)'),
                       br(),
                       h3("Administrative Specialist"),
                       p('Paulette Smith  (Email: paulettes@nezperce.org)')
                   ))
          ),
          box(title = 'Project Information', solidHeader = TRUE, width = 12, status = 'info', 
              uiOutput(outputId = 'research_select'),
              h3("Description"),
              uiOutput('research_description'), br(),br(),
              h3('Objectives'),
              uiOutput('research_objectives'), br(),br(),
              h3('Project Leader'),
              uiOutput('research_PL'), br(),br(),
              h3('Staff'),
              uiOutput('research_staff')
          )
  ),
  
  # Watershed Tab ----
  tabItem('tab_watershed', br(),
          h2('Watershed Information', style = 'text-align:center; color:white; font-family: Arial White;'),
          fluidRow(
            column(10, offset = 1,
                   box(status = 'info', width=12, background = 'aqua',
                       p("Watershed Description Here!")
                   )),
            column(6, offset = 3,
                   box(title = NULL, width = 12, status = 'info', 
                       h3("Division Director"),
                       p('Emmit Taylor  (Email: emmitt@nezperce.org)'),
                       br(),
                       h3("Administrative Specialist"),
                       p('Ermie Whitman  (Email: ermiew@nezperce.org)')
                   ))
          )
  ),
  
  # Conservation Enforcement Tab ----
  tabItem('tab_conservation', br(),
          h2('Conservation Enforcement Information', style = 'text-align:center; color:white; font-family: Arial White;'),
          fluidRow(
            column(10, offset = 1,
                   box(status = 'info', width=12, background = 'aqua',
                       p("Conservation Description Here!")
                   )),
            column(6, offset = 3,
                   box(title = NULL, width = 12, status = 'info', 
                       h3("Division Director"),
                       p('Adam Villavicencio  (Email: adamv@nezperce.org)'),
                       # br(),
                       # h3("Administrative Specialist"),
                       # p('  (Email: @nezperce.org)')
                   ))
          )
  ),
  
  # Spawning Ground Survey Summaries Tab ----
  tabItem(tabName = 'tab_sgs',
          fluidRow(
            column(12,
            box(title = 'Spawning Ground Survey Summaries', status='info', width= 5,
                uiOutput(outputId = 'sgs_species'),
                uiOutput(outputId = 'sgs_pop_name')
            ),
          box(width = 7, 
              img(src='carcass.png', width = '100%', height='auto') 
                ))
          ),
          hr(),
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
              fluidRow(column(12, align = "center", downloadButton("sgs_export", label = "Export .CSV File")),
              div(style = 'overflow-x: scroll;', DT::dataTableOutput('sgs_table')))
          )
  ),
  
  # Weir Collections Summaries Tab ----
  tabItem(tabName = 'tab_weir',
          fluidRow(
            column(12,
                   box(title = 'Weir Collection Summaries', status='info', width= 5,
                       uiOutput(outputId = 'weir_species'),
                       uiOutput(outputId = 'weir_trap')
                   ),
                   box(width = 7, 
                       img(src='jcweir.jpg', width = '100%', height='auto') 
                   ))
          ),
          hr(),
          fluidRow(
            box(width = 12, title = paste(year(Sys.Date()), ' Weir Catch Summary', sep = ''),
                fluidRow(column(12, align = "center", downloadButton("weirsum_export", label = "Download Summary (.CSV)"))),
                div(style = 'overflow-x: scroll;', DT::dataTableOutput('weir_sum_all')))
          ),
          hr(),
          fluidRow(
            box(width = 12, plotlyOutput('p_weircatch_N'), height = '600')
          ),
          fluidRow(
            box(width = 12, plotlyOutput('p_weircatch_H'), height = '600')
          ),
          box(width = 12, 
              title = paste(year(Sys.Date()), 'Weir Collections Summary'),
              fluidRow(column(12, align = "center", downloadButton("weir_export", label = "Export .CSV File"))),
              div(style = 'overflow-x: scroll;', DT::dataTableOutput('weir_table'))
          )
  ),
          
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
                # uiOutput(outputId = 'juv_data_button'),
                uiOutput(outputId = 'juv_species'),
                uiOutput(outputId = 'juv_pop_name')
            ),
            box(width = 7, 
                img(src='lostine_rst.jpg', width = '100%', height='auto') 
            ))
          ),
          hr(),
          fluidRow(
            box(width = 12, plotlyOutput('j_abundance'))
                  ),
          fluidRow(
            box(width = 6, plotlyOutput('j_survival'),
                helpText('Solid Lines = Natural Origin; Dashed Lines = Hatchery Origin.', style= 'text-align:center;')),
            box(width = 6, plotlyOutput('j_equivalents'),
                br(), br())
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
              ),
  
  # Documents Tab ----
  tabItem(tabName = 'tab_documents',
          fluidRow(
            box(width = 12, status = "info",
                h2('CDMS Document Access', style = 'text-align:center;'),
                uiOutput(outputId='documents_info'),
                DT::dataTableOutput('documents_table')
            ) 
          )
  ),
  
  # Restricted Data Access Tab ----
    # CDMS Datasets ----
      tabItem(tabName = 'tab_cdms',
              
              box(width = 12, 
              fluidRow(column(6, uiOutput("raw_dataset_menu"),
                              fluidRow(
                                column(8, offset = 2, actionButton("raw_submit", label = "Load Selected Dataset", icon = icon('hourglass-start'), width = '100%'))
                                      ),
                              br(),
                              selectInput(inputId = 'q_fields', label = 'Choose Fields in Desired Order:', choices = NULL, selectize = TRUE, multiple = TRUE),
                              sliderInput(inputId= 'q_year', label= '*Choose Years:', min = 0, max = 100, value=  c(0,100), sep= '', step = 1),
                              helpText(HTML('<em>* Year is "Spawn Year" for adult datasets, "Migratory Year" for juvenile datasets, and "Collection Year" for Age data.</em>'), style = 'text-align:center;')
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
              fluidRow(column(12, align = "center", 
                              uiOutput('selected_cdms'), hr(),
                              downloadButton("raw_export", label = "Export .CSV File"))),
              div(style = 'overflow-x: scroll;', DT::dataTableOutput('raw_table'))
              )
      ),
    # Custom Queries ----
      tabItem(tabName = 'tab_custom',
                box(width = 12,
                    h4('This page is intended to meet the needs of Projects and Biologists to produce desired data views or 
                             summaries.', style = 'text-align: center;'),
                    h4('Please contact Tyler Stright (tylers@nezperce.org) with inquiries.', style = 'text-align: center;'),
                    hr(),
                    fluidRow(column(6, 
                                    selectInput('custom_query_menu', label = NULL, choices = query_names, selected = '-Select Custom Query-'),
                                    uiOutput('query_description', style = 'text-align:center;'),
                                    br(),
                                    uiOutput('custom_query_grouping', label = NULL),
                                    uiOutput('groupingtext', label ='GRPTXT'),
                                    br(),
                                    fluidRow(
                                      column(8, offset=2, actionButton("custom_submit", label = "Submit Query", icon = icon('hourglass-start'), width = '100%'))
                                            )
                                    ),
                             column(6, 
                                    selectInput(inputId = 'cq_fields', label = NULL, choices = NULL, selectize = TRUE, multiple = TRUE),
                                    helpText(HTML('<em>Select desired fields in preferred order.</em>'), style='text-align:center;')
                                    )
                    )  
                ),
              box(width = 12, 
                  fluidRow(column(12, align = "center", 
                                  uiOutput('selected_custom'),
                                  downloadButton("custom_export", label = "Export .CSV File"))),
                  div(style = 'overflow-x: scroll;', DT::dataTableOutput('custom_table'))
              )
        ),
    # FINS Data ----
      tabItem(tabName = 'tab_fins',
              box(width = 12,
                  h4("This page is still currently under development and will be updated with
                     increased functionality in the future.", style='text-align:center;'),
              # box(width = 12,
                  fluidRow(column(6, offset = 3, br(), #uiOutput("fins_menu"),
                                  # fluidRow(
                                  #   column(6, actionButton("fins_raw", label = "Load Raw FINS Data", icon = icon('hourglass-start'), width = '100%')),
                                  #   column(6, actionButton("fins_clean", label = "Load Cleaned FINS Data", icon = icon('hourglass-start'), width = '100%'))
                                  # ),
                  br(),
                  radioButtons(inputId = 'fins_filtertype', label = 'Filter by Facility or Year? *', choices = c('Facility', 'Year'), 
                               inline = TRUE, selected = 'Facility'),
                  uiOutput('fins_filter')
                  # selectInput(inputId = 'fins_fields', label = 'Choose Fields in Desired Order:', choices = NULL, selectize = TRUE, multiple = TRUE),
                  # sliderInput(inputId= 'fins_year', label= '*Choose Years:', min = 0, max = 100, value=  c(0,100), sep= '', step = 1)
                  ),
                  # column(6,
                         # selectInput(inputId= 'fins_species', label= 'Choose Species:', choices= NULL, selected = NULL, multiple = TRUE),
                         # selectInput(inputId= 'fins_facility', label= 'Choose Facility:', choices= NULL, selected = NULL, multiple = TRUE),
                         # br(),
                         # fluidRow(
                         #   column(8, offset = 2, actionButton('fins_clear_fields', HTML('<strong> Clear Field Values </strong>'), width = '100%'))
                         # )
                  # )
                  ),
                  # hr(),
                  fluidRow(column(12, align = "center",
                                  # uiOutput('selected_fins'), 
                                  hr(),
                                  downloadButton("fins_export", label = "Export .CSV File")))#,
              #     div(style = 'overflow-x: scroll;', DT::dataTableOutput('fins_table'))
              )
      ),
    # Reports ----
      tabItem(tabName = 'tab_reports',
              fluidRow(
                box(width = 12,
                    h2('Reports!', style = 'text-align: center;'),
                    h3('This page is itended to be used for automated reports. If you create the same report on a consistent basis (e.g. similar graphs and tables of information),
                       we can work together to automate these reports so they are available at the click of a button with the most current data in CDMS.', style='text-align:center;'),
                    h4('Please contact Tyler Stright (tylers@nezperce.org) with inquiries.', style = 'text-align: center;')
                    )
              ),
              fluidRow(
                box(width = 12,
                    title = 'Report Download',
                    uiOutput('pdf_reports'),
                    # helpText(HTML('<em>*Reports are generated from raw data at the time of request. As such, loading may take several minutes. Clicking the download button multiple times may result in multiple downloads.</em>')),
                    downloadButton('report_export', label = 'Download Report')
                    )
              )
          ),
  
      tabItem('tab_contacts',
          h1('Contact Information', style = 'text-align:center; color:white; font-family: Arial White;'),
                br(),
                h2('Field Office Contacts', style = 'color:white'),
                h4('Joseph Field Office: Jim Harbeck  (541) 432-2501', style = 'color:white'),
                h4('McCall Field Office: Ryan Kinzer  (208) 634-5290', style = 'color:white'),
                h4('Orofino Field Office: Ryan Kinzer  (208) 634-5290', style = 'color:white'),
                h4('Sweetwater Office: Bill Arnsburg  (208) 476-7417', style = 'color:white'),
                hr(),
                h2('Division Contacts', style = 'color:white'),
                h4('Administration: Dave Johnson (Email: davej@nezperce.org)', style = 'color:white'),
                h4('Harvest: Joe Oatman (Email: joeo@nezperce.org)', style = 'color:white'),
                h4('Production: Becky Johnson (Email: beckyj@nezperce.org)', style = 'color:white'),
                h4('Research: Jason Vogal (Email: jasonv@nezperce.org)', style = 'color:white'),
                h4('Conservation Enforcement: Adam Villavicencio (Email: adamv@nezperce.org)', style = 'color:white'),
                hr(),
                h2('Data Management Contacts', style = 'color:white'),
                h4('Clark Watry (Email: clarkw@nezperce.org)', style = 'color:white'),
                h4('Ryan Kinzer (Email: ryank@nezperce.org)', style = 'color:white'),
                h4('Samantha Smith (Email: samanthas@nezperce.org)', style = 'color:white'),
                h4('Tyler Stright (Email: tylers@nezperce.org)', style = 'color:white'),
                hr(),
                h2('Kus Web App Contacts', style = 'color:white'),
                h4('Clark Watry (Email: clarkw@nezperce.org)', style = 'color:white'),
                h4('Tyler Stright (Email: tylers@nezperce.org)', style = 'color:white')
      )
  
    ) #tabItems
  ) #dashboardBody

dashboardPage(title = "Nez Perce Tribe's Kus Web Application", header, sidebar, body, skin= 'blue')
