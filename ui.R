# Kus UI

# DashboardHeader ----
header <- dashboardHeader(title = div(id = "kustitle", 'Kus'),
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
                            class = "dropdown")
                          )

# Dashboard Sidebar ----
sidebar <- dashboardSidebar(
    useShinyjs(), # Activate ShinyJS
    sidebarMenu(
      menuItem('Kus Home', tabName = 'tab_home', icon = icon("home")),
      menuItem('Data Summaries', tabName = 'tab_productivity', icon = icon("chart-area"), startExpanded = TRUE,
               menuSubItem('Spawning Ground Surveys'),
               menuSubItem('Weir Collections'),
               menuSubItem('In-stream Array Abundance'),
               menuSubItem('Juvenile Monitoring')
               ),
      menuItem('Restricted Data Access', tabName = 'tab_rawdata', icon = icon('table'), startExpanded = TRUE,
               menuSubItem('CDMS Datasets'),
               menuSubItem('Project Queries'),
               menuSubItem('Reports')
               ),
      menuItem('Contact Us')
    )
  )

# Dashboard Body ----
body <- dashboardBody(
  includeCSS('./www/styles.css'),
    tabItems(
  # KusHome Tab ----
      tabItem(tabName = 'tab_home',
              # fluidRow(
              #   valueBoxOutput("sgs_totalmiles", width = 4),
              #   valueBoxOutput("sgs_groundmiles", width = 4),
              #   valueBoxOutput("sgs_airmiles", width = 4)
              # ),
              # fluidRow(
              #   box(status = 'info', 
              #       # title = paste('Data Repositories (', dataset_count2, '):'), # working here Monday
              #       background = 'blue',
              #       solidHeader = TRUE, 
              #       collapsible = TRUE, 
              #       collapsed = TRUE,
              #       width = 3, 
              #       tableOutput('dataset_dt2'))
              # ),
              fluidRow(
                column(8, p('The Kus web application is intended to provide near real-time data summaries and
                  visualizations to the general public, Nez Perce Tribal members, and internal Research Division staff.  
                  The tools also create easy access to valuable fish information for the public and will provide the 
                  most current and best fishing oppurtunities to tribal fishermen.  Additionally, these provided resources 
                  will help inform the Department of Fisheries Resources Management staff and
                  support Snake Basin fisheries management decisions.')),
                column(4, img(src='sunk_trap.jpg', class = 'pic'))
              ),
              fluidRow(div(br(), style= 'border-bottom: 1px solid rgb(0, 0, 0);'), br()), # clever spacer
              fluidRow(height = '200px', width = '90%',
                    column(3, img(src='goldfish.jpg', class='pic')),
                    column(9, br(), h2(HTML("Chinook salmon : <em>nacό’x</em>")), 
                           p('Chinook salmon (Oncorhyncus tshawytscha) are the largest salmon species in the Columbia basin. Historical accounts report adults reaching 100 pounds.  As anadromous fish, Chinook migrate to the ocean as juveniles, spend several years in the ocean during adulthood, and return in the Fall to their natal streams to spawn and die.',
                           'The Nez Perce Tribe have long relied on fish as a source of food, but none so much as the Chinook salmon. Chinook are also an important cultural and spiritual symbol, to the extent that the time of year was measured by the Chinook’s life cycle.  This reality is reflected in Nez Perce stories, legends, and ceremonies.')
                    )
              ),
              fluidRow(div(br(), style= 'border-bottom: 1px solid rgb(0, 0, 0);'), br()), # clever spacer
              fluidRow(height = '200px', width = '90%',
                    column(9, br(), h2(HTML('Steelhead trout : <em>héyey</em>')),
                           p('Steelhead (Oncorhyncus mykiss) are another fish native to the Columbia and Snake River as well as an important food source for the tribe.  Unlike Chinook salmon, Steelhead regularly spend only one year in the ocean as adults before returning tin the Spring to spawn in their natal freshwater streams.  In some cases, the adults do not die after spawning and are able to spawn twice in their lifetime, a behavior known as iteroparity.  However, others choose not migrate to the ocean and are known as Rainbow trout.')
                    ),
                    column(3, img(src='goldfish.jpg', class='pic'))
              ),
              fluidRow(div(br(), style= 'border-bottom: 1px solid rgb(0, 0, 0);'), br()), # clever spacer
              fluidRow(height = '200px', width = '90%',
                    column(3, img(src='goldfish.jpg', class='pic')),
                    column(9, br(), h2(HTML('Coho salmon : <em>kállay</em>')), 
                           p('Coho salmon (Oncorhyncus kisutch) are another anadromous fish native to the Columbia Basin.  Dam construction on the Columbia and Snake rivers eventually led to the Snake River Coho runs being declared extinct in 1987 (Source?).  Coho restoration efforts are underway in attempt to restore and reestablish these salmon in the waters where they once flourished.  In 2017, juvenile Coho were reintroduced to the Wallowa River, and for the first time since 1987, adult Coho returned to the Wallowa River in 2018.')
                    )
              ),
              fluidRow(div(br(), style= 'border-bottom: 1px solid rgb(0, 0, 0);'), br()), # clever spacer
              fluidRow(height = '200px', width = '90%',
                    column(9, br(), h2(HTML('Pacific Lamprey : <em>hésu</em>')),
                           p('Pacific lampreys (Lampetra tridentate) are the most common lampreys in the Columbia Basin.  As juveniles, they spend up to six years in freshwater streams filtering food from the water until they are ready to migrate to the ocean.  During their adult life, the eels become parasitic, can reach lengths of up to thirty inches, and prey on species such as Chinook salmon. As yet another anadromous fish, Lamprey return to their natal freshwater streams to spawn and die.'),
                           p('Many tribes used eels as a food source (dried or smoked) and considered them as valuable as Salmon.'),
                           p('Recently, the tribe has begun lamprey relocation, taking eels captured at various dams and trucking them to be released high up in freshwater streams to spawn.  Efforts to develop a hatchery program for Pacific Lamprey have also begun.')
                    ),
                    column(3, img(src='goldfish.jpg', class='pic'))
              )
            ),
              # fluidRow(
              #   column(4,
              #   infoBoxOutput("project_count", width = 12),
              #     bsModal(id="project_count_modal", title = "Nez Perce Tribe Research Projects:", tableOutput('project_dt'), trigger=""),
              #   infoBoxOutput("dataset_count", width = 12),
              #     bsModal(id="dataset_count_modal", title = "Nez Perce Tribe Datastores:", tableOutput('dataset_dt'), trigger="")
              #   )
              # ),
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
