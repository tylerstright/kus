# Kus UI

# DashboardHeader ----
header <- dashboardHeader(title = "Kus",
                          #span("Kus", style = "color: red; font-size: 28px; font-weight:bold; font-family: Arial "),
                          tags$li(tags$a("NPT Fisheries Home", href= 'http://www.nptfisheries.org', 
                                         style = "color: white; float:left;", target ='_blank'), class = 'dropdown', style= 'position:absolute; left:42px'),
                          tags$li(tags$a("CDMS - Project Data Entry", href = "https://cdms.nptfisheries.org/index.html#/projects", target = '_blank'), 
                                  class = 'dropdown', style = 'position:absolute; left:190px;'),
                          tags$li(tags$a("LSRCP FINS", href = "https://www.finsnet.org/", target = '_blank'), 
                                  class = 'dropdown', style = 'position:absolute; left:370px;'),
                          tags$li(tags$a("PITPH Web App", href = "https://nptfisheries.shinyapps.io/pitph2/", target = '_blank'), 
                                  class = 'dropdown', style = 'position:absolute; left:467px;'),
                          tags$li(tags$a("PITtrackR Web App", href = "https://nptfisheries.shinyapps.io/PITtrackR/", target = '_blank'), 
                                  class = 'dropdown', style = 'position:absolute; left:587px;'),
                          # Login/Logout Link
                          tags$li(class = 'dropdown', style = 'font-weight: 500; font-size: 11pt; 
                                  padding-top:15px; padding-bottom: 10px; position:absolute; right:90px; color: white;',
                                  uiOutput('login_logout')), 
                          # Logo
                          tags$li(img(src = 'NPTlogos2.png', title = 'Company Home', height = "30px"),
                            style = 'position:absolute; right:15px; padding-top:10px; padding-bottom:10px;',
                                  # style = "padding-top:10px; padding-bottom:10px; padding-right:15px;",
                            class = "dropdown")
  )

# Dashboard Sidebar ----
sidebar <- dashboardSidebar(
    useShinyjs(), # Activate ShinyJS
    sidebarMenu(
      # menuItem(div(style="display:inline-block;width:90%;text-align: center;", img(src="NPTlogos2.png", height = "90px")),
      #          uiOutput('login_logout')),
      # menuItem("Species Selector:", icon = icon('fish', lib = 'font-awesome'), tabName = 'tab_inputs',
      #   selectInput('species', NULL, choices = c('Fall Chinook salmon', 'Spring/summer Chinook salmon', 
                                                # 'Summer Steelhead'), selected = 'Spring/summer Chinook salmon')),
      menuItem('Kus Home', tabName = 'tab_home', icon = icon("home")),
      # menuItem('Adult Summaries', tabName = 'tab_adult', icon = icon("chart-bar")),
      # menuItem('Juvenile Summaries', tabName = 'tab_juvenile', icon = icon("chart-line")),
      # menuItem('Productivity Summaries', tabName = 'tab_productivity', icon = icon("chart-area")),
      menuItem('Raw Data Access', tabName = 'tab_rawdata', icon = icon('table'))
    )
  )

# Dashboard Body ----
body <- dashboardBody(
    tabItems(
  # KusHome Tab ----
      tabItem(tabName = 'tab_home',
              fluidRow(
                valueBoxOutput("sgs_totalmiles", width = 4),
                valueBoxOutput("sgs_groundmiles", width = 4),
                valueBoxOutput("sgs_airmiles", width = 4)
              ),
              fluidRow(
                column(8, leafletOutput('home_map', height = '600px')),
                valueBoxOutput("project_count", width = 4),
                  bsModal(id="project_count_modal", title = "Nez Perce Tribe Research Projects:", tableOutput('project_dt'), trigger=""),
                infoBoxOutput("dataset_count", width = 4),
                  bsModal(id="dataset_count_modal", title = "Nez Perce Tribe Datastores:", tableOutput('dataset_dt'), trigger=""),
                infoBox('UNUSED BOX 1', "Skibbadee"),
                infoBox('UNUSED BOX 2', "Bibbadee"),
                infoBox('UNUSED BOX 3', 'BOO!')
              ),
              fluidRow(
                column(12,
                       helpText('The Kus web application is intended to provide near real-time data summaries and
                                visualizations for Department of Fisheries Resources Management staff and to
                                support Snake Basin fisheries management decisions. The tools also creates easy access to
                                valuable fish information for general public consumption and informs Nez Perce
                                Tribal members of the most current and best fishing oppurtunities.',  
                                width = '100%')
                )
                )
      ),
  # Adult Summaries Tab ----
      # tabItem(tabName = 'tab_adult',
      #         fluidRow(
      #           box(title = "Total Redds by Year", width = 12,
      #               plotlyOutput('p_redds'))
      #         ),
      #         fluidRow(
      #           box(title = "Total Carcasses by Year", width = 12,
      #               plotlyOutput('p_carcass'))
      #         ),
      #         fluidRow(
      #           box(title = "Escapement by Year", width = 6),
      #           box(title = "Adult Age Composition", width = 6)
      #         )
      # ),
  # Juvenile Summaries Tab ----
      # tabItem(tabName = 'tab_juvenile',
      #         fluidRow(
      #           box(title = "Juvenile Abundance Estimates", width = 12,
      #               plotlyOutput('j_abundance'))
      #         ),
      #         fluidRow(
      #           box(title = "Survival Estimates to Lower Granite Dam", width = 12,
      #               plotlyOutput('j_survival'))
      #         )#,
      #         # fluidRow(
      #         #   box(title = "Smolt Equivalents", width = 6),
      #         #   box(title = "Hatchery Releases", width = 6)
      #         # )
      #         ),
  # Productivity Summaries Tab ----
      tabItem(tabName = 'tab_productivity'),
  # Raw Data Access Tab ----
      tabItem(tabName = 'tab_rawdata',
              fluidRow(column(3, uiOutput("raw_dataset_menu"))
              ),
              fluidRow(column(6, helpText("Select the desired dataset then the project, stream and locations of interest and click submit query.")),
                       column(2, offset = 2, align = "center",
                              actionButton("raw_submit", label = "Submit Query", class = "mybutton")),
                       column(2, align = "center",
                              downloadButton("raw_export", label = "Export .CSV File", class = "mybutton"))
                       ),
              hr(),
              withSpinner(DT::dataTableOutput('raw_table'))
      )
    ) #tabItems
  ) #dashboardBody

dashboardPage(header, sidebar, body)
