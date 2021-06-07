# Kus UI

# DashboardHeader ----
header <- dashboardHeader(title = div(id = "kustitle", 'Kus', style='float:right;'),  # Title shown on browser tab located in dashboardPage()
                          tags$li(img(src='NPTlogos2.png', title = NULL, draggable = FALSE, style = 'height:40px !important; width:auto !important;'), 
                                  class = 'dropdown', style = 'position: fixed; left:40px; padding-top:6px'),
                          tags$li(tags$a("Snake IPTDS", href = "https://nptfisheries.shinyapps.io/SnakeIPTDS/", target = '_blank', class='navlinks'),
                          class = 'dropdown'),
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
                       menuSubItem('Conservation Enforcement', tabName = 'tab_enforcement')
              ),
              menuItem('Data Summaries', tabName = 'tab_productivity', icon = icon("chart-area"), startExpanded = TRUE,
                       menuSubItem('Spawning Ground Surveys', tabName = 'tab_sgs'),
                       menuSubItem('Weir Collections', tabName = 'tab_weir'),
                       menuSubItem('Spawner Abundance', tabName = 'tab_nosa'),
                       menuSubItem('Hatchery Spawning', tabName = 'tab_spawn'),
                       menuSubItem('Fall Chinook', tabName = 'tab_fchn'),
                       # menuSubItem('In-Stream Array Abundance', tabName = 'tab_array'),
                       menuSubItem('Juvenile Monitoring', tabName = 'tab_juv'),
                       menuSubItem('Rotary Screw Traps', tabName = 'tab_rst')
                       # menuSubItem('Age Sampling', tabName = 'tab_age')
              ),
              menuItem('Documents', tabName = 'tab_documents', icon = icon("file")),
              menuItem('Restricted Data Access', tabName = 'tab_rawdata', icon = icon('table'), startExpanded = TRUE,
                       menuItemOutput('rd_cdms'),
                       menuItemOutput('rd_customquery'),
                       menuItemOutput('rd_fins'),
                       menuItemOutput('rd_reports')
              ),
              menuItem('Contact Information', tabName = 'tab_contacts'),
              br(), br(), 
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
              column(12, offset = 0,
                     h1('Department of Fisheries Resources Management', style = 'text-align:center;'),
                     br(),
                     column(10, offset = 1,
                            box(status = 'info', width=12, background = 'aqua',
                                p("The Nez Perce Department of Fisheries Resources Management will protect and restore aquatic resources and habitats. Our mission will be accomplished consistent with the Nimmipuu way of life and beliefs, which have the utmost respect for the Creator, for all species, and for the past, present and future generations to come. Our mission will be consistent with the reserved rights stated within the Nez Perce Tribe's 1855 Treaty."),
                            )),
                     br()
              )#,
              # column(2,
                     # tags$a(
                     #   href = 'https://npt-cdms.nezperce.org/services/uploads/P/11066/DFRM%20Management%20Plan.pdf',
                     #   target = '_blank',
                     #   tags$img(src='DFRM Management Plan Cover.jpg',
                     #            title = 'Click to view the DFRM Management Plan',
                     #            style = 'width:80% !important; padding-bottom:10%')
                     # )
              # )
            ),
            
            box(width = NULL, solidHeader = TRUE, status = 'primary',
                title = 'Lower Granite Window Counts',
                fluidRow(
                  windowCountUI('chinook'),
                  windowCountUI('steelhead'),
                  windowCountUI('coho'),
                  windowCountUI('sockeye')
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
                         p("Administration Description Here.")
                     )),
              column(6, offset = 3,
                     box(title = NULL, width = 12, status = 'info',
                         employeeInfoUI('administration_director'),
                         employeeInfoUI('administration_assistant')
                     ))
            ),
            divInfoUI(id='administration')
    ),
    
    # Harvest Tab ----
    tabItem('tab_harvest', br(),
            h2('Harvest Information', style = 'text-align:center; color:white; font-family: Arial White;'),
            fluidRow(
              column(10, offset = 1,
                     box(status = 'info', width=12, background = 'aqua',
                         p("The Nez Perce Tribe intends to increase and expand the level of harvest or fishing areas for salmon and steelhead at all Nez Perce usual and accustomed areas in the
Snake Basin in a way that balances conservation needs of the fish with the right to take fish. This can be achieved through a biologically-sound harvest management philosophy
and harvest rate schedules keyed to the status and trends in abundance and productivity of the fishery resource. ")
                     )),
              column(6, offset = 3,
                     box(title = NULL, width = 12, status = 'info',
                         employeeInfoUI('harvest_director')
                     ))
            ),
            divInfoUI(id='harvest')
    ),
    
    # Production Tab ----
    tabItem('tab_production', br(),
            h2('Production Information', style = 'text-align:center; color:white; font-family: Arial White;'),
            fluidRow(
              column(10, offset = 1,
                     box(status = 'info', width=12, background = 'aqua',
                         p("The Nez Perce Tribe continues to protect and enhance abundance of fish through natural production and artificial production in the form of hatcheries. Hatcheries for salmon and steelhead in the Columbia Basin were developed as a necessary mitigation tool to compensate
for the fishery losses that resulted from the impacts of increased human settlement that began soon after ratification of the Treaty of 1855. Accordingly,
hatcheries represent a promise to those who have always depended on the salmon for culture, sustenance, and livelihood to replace the fish that are and
were diminished as a result of human development of salmon habitats. As long as the dams are here, the mitigation responsibility remains.")
                     )),
              column(6, offset = 3,
                     box(title = NULL, width = 12, status = 'info',
                         employeeInfoUI('production_director'),
                         employeeInfoUI('production_assistant')
                     ))
            ),
            divInfoUI(id='production')
    ),
    
    # Research Tab ----
    tabItem('tab_research', br(),
            h2('Research Information', style = 'text-align:center; color:white; font-family: Arial White;'),
            fluidRow(
              column(10, offset = 1,
                     box(status = 'info', width=12, background = 'aqua',
                         p("The research division is responsible for gathering the data necessary to assess the success of operations in achieving
                         the biological, physical, and harvest management goals described in the DFRM Management Plan.  Only with consistent research
                         and diligently recorded data will the DFRM be able to effectively implement an adaptive management strategy.  Adaptive management consists of
monitoring the results of actions, evaluating their effectiveness, adjusting plans if necessary, and applying new or modified strategies from knowledge gained.")
                     )),
              column(6, offset = 3,
                     box(title = NULL, width = 12, status = 'info',
                         employeeInfoUI('research_director'),
                         employeeInfoUI('research_assistant')
                     ))
            ),
            divInfoUI(id='research')
    ),
    
    # Watershed Tab ----
    tabItem('tab_watershed', br(),
            h2('Watershed Information', style = 'text-align:center; color:white; font-family: Arial White;'),
            fluidRow(
              column(10, offset = 1,
                     box(status = 'info', width=12, background = 'aqua',
                         p("Native fish within the Nez Perce Country depend
on healthy habitats, healthy watersheds, and healthy ecosystems. At the most fundamental level, both resident and anadromous species require: clean, cold and oxygen-rich flows; adequate stream depths
to avoid predation and allow seasonal movement. The health of entire watersheds, from ridge-top to ridge-top, is important for fish survival because
watersheds contain an interconnected web of life. Water that falls as rain or snow flows down slope
across the landscape and through the ground before it eventually enters a common stream or other body
of water. This defines the spatial extent of a watershed.")
                     )),
              column(6, offset = 3,
                     box(title = NULL, width = 12, status = 'info',
                         employeeInfoUI('watershed_director'),
                         employeeInfoUI('watershed_assistant')
                     ))
            ),
            divInfoUI(id='watershed')
    ),
    
    # Conservation Enforcement Tab ----
    tabItem(tabName = 'tab_enforcement', br(),
            h2('Conservation Enforcement Information', style = 'text-align:center; color:white; font-family: Arial White;'),
            fluidRow(
              column(10, offset = 1,
                     box(status = 'info', width=12, background = 'aqua',
                         p("The Enforcement program enforces the Nez Perce tribal regulations regarding the fish
and wildlife portion of the Law and Order Code. As a sovereign, the Nez Perce Tribe has the ability and
responsibility to regulate the activities of its membership and, in general, the activities occurring on
its lands. Especially with regards to the resource conservation issues surrounding the exercise of fishing rights, the Nez Perce Tribe must have the ability
to set and enforce its seasons in order to fish without state interference. So it is not only for purposes
of resource protection, but also for the conducting of its activities as a sovereign that the Enforcement
program and its staff serve a key role.")
                     )),
              column(6, offset = 3,
                     box(title = NULL, width = 12, status = 'info',
                         employeeInfoUI('enforcement_director'),
                     ))
            ),
            divInfoUI(id='enforcement')
    ),
    
    # Spawning Ground Survey Summaries Tab ----
    tabItem(tabName = 'tab_sgs',
            fluidRow(
              column(12,
                     box(title = 'Spawning Ground Survey Summaries', status='info', width= 5, #height = '260',
                         uiOutput(outputId = 'sgs_species'),
                         uiOutput(outputId = 'sgs_pop_name')
                     ),
                     box(width = 7, #height = '260',
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
                # fluidRow(column(12, align = "center", downloadButton("sgs_export", label = "Export .CSV File"))),
                DT::dataTableOutput('sgs_table')
            )
    ),
    
    # Weir Collections Summaries Tab ----
    tabItem(tabName = 'tab_weir',
            fluidRow(
              box(width = 12, title = paste(year(Sys.Date()), ' Weir Catch Summary* - Chinook', sep = ''),
                  helpText(HTML('<em>* Only includes new catch - recaptures are excluded.</em>')),
                  DT::dataTableOutput('weir_sum_chn'), 
                  br())
            ),
            fluidRow(
              box(width = 12, title = paste(year(Sys.Date()), ' Weir Catch Summary* - Steelhead', sep = ''),
                  helpText(HTML('<em>* Only includes new catch - recaptures are excluded.</em>')),
                  DT::dataTableOutput('weir_sum_sth'), 
                  br())
            ), hr(),
            fluidRow(
              column(12,
                     box(title = 'Weir Collection Summaries', status='info', width= 5,
                         uiOutput(outputId = 'weir_species'), 
                         uiOutput(outputId = 'weir_trap'),
                         uiOutput(outputId = 'weir_year')
                     ),
                     box(width = 7, 
                         img(src='jcweir.jpg', width = '100%', height='auto') 
                     ))
            ), hr(),
            fluidRow(
              box(width = 12, plotlyOutput('p_weircatch'))
            ), hr(),
            fluidRow(
              box(width = 6, title = 'Weir Collection Statistics**',
                  helpText(HTML('<em>** Recaptures excluded.</em>')),
                  dataTableOutput('weir_props_table')),
              box(width = 6,
                  plotlyOutput('p_weir_props'))
            ), hr(),
            box(width = 12, 
                title = 'Weir Disposition Summary***',
                helpText(HTML('<em>*** Recaptures included.</em>')),
                # fluidRow(column(12, align = "center", downloadButton("weir_export", label = "Export .CSV File"))),
                DT::dataTableOutput('weir_table')
            )
    ),
    # Spawner Abundance Tab ----
    tabItem(tabName = 'tab_nosa',
            fluidRow(
              column(12,
                     box(title = 'Spawner Abundance Estimates', status='info', width= 5, #height = '260',
                         uiOutput(outputId = 'nosa_method'),
                         uiOutput(outputId = 'nosa_species'),
                         uiOutput(outputId = 'nosa_waterbody'),
                         textOutput(outputId = 'methodtext')
                     ),
                     box(width = 7, #height = '260',
                         img(src='nosa.png', width = '100%', height='auto')
                     ))
            ),
            hr(),
            fluidRow(
              box(width = 12, plotlyOutput('p_nosaij'))
            ),
            box(width = 12,
                title = 'Tabular Summary Data',
                DT::dataTableOutput('nosa_table')
            )
    ),
    # Hatchery Spawning (FINS) Tab ----
    tabItem(tabName = 'tab_spawn',
            fluidRow(
              column(12,
                     box(title = 'Hatchery Spawning Summary', status='info', width= 5, #height = '260',
                         uiOutput(outputId = 'spawn_species'),
                         uiOutput(outputId = 'spawn_facility')
                     ),
                     box(width = 7, #height = '260',
                         img(src='spawn.png', width = '100%', height='auto')  # update photo
                     ))
            ),
            hr(),
            fluidRow(
              box(width = 12, plotlyOutput('p_spawn'))
            ),
            box(width = 12,
                title = 'Tabular Summary Data',
                DT::dataTableOutput('spawn_table')
            )
    ),
    # Fall Chinook Run Reconstruction Data Summaries Tab ----
    tabItem(tabName = 'tab_fchn',
            fluidRow(
              column(12, 
                     box(title = 'Fall Chinook Yearly Escapement Estimates by Origin', 
                         status = 'info', width = 12,
                         plotlyOutput(outputId = 'fchn_esc'))
              )),
            fluidRow(
              column(12, 
                     box(title = NULL, # 'Age Composition by Year', 
                         status = 'info', width = 6,
                         plotlyOutput(outputId = 'fchn_age')),
                     box(title = NULL, # 'Sex Composition for All Years', 
                         status = 'info', width = 6,
                         plotlyOutput(outputId = 'fchn_sex'))
              )
            )
    ),
    # In-Stream Array Abundance Summaries Tab ----
    # tabItem(tabName = 'tab_array',
    #         fluidRow(
    #           box(title = 'In-Stream Array Summaries',
    #               helpText('Sorry! This page is currently under construction.'))
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
                # fluidRow(column(12, align = "center", downloadButton("juv_export", label = "Export .CSV File"))),
                DT::dataTableOutput('juv_table')
            )
    ),
    
    # Rotary Screw Traps Tab ----
    tabItem(tabName = 'tab_rst',
            fluidRow(
              column(12, 
                     box(title = 'Rotary Screw Trap Summary', status='info', width= 5,
                         h3('This page is currently under construction. Stay tuned!'),
                         uiOutput('rst_trap')
                         # DISCHARGE? 
                         # Days No Catch / Trap not operational
                         # Daily Catch
                         # Mortality Count
                         # Daily Efficiency?
                         # Fish sent upstream v. recaps
                         # Tags issued
                         # Date of first fish?
                         # Hours fished per day?
                     ),
                     box(width = 7, 
                         img(src='lostine_rst.jpg', width = '100%', height='auto') # Should we get a new picture?
                     ))
            ),
            # hr(),
            # fluidRow(
            #   box(width = 12, plotlyOutput('rst_dailycatch'))
            # ), br(),
            # fluidRow(
            #   box(width = 6, plotlyOutput('')),
            #   box(width = 6, plotlyOutput(''),
            #       br(), br())
            # ),
            
            box(width = 12, title = 'Catch Summary by Hitch (Thursday - Wednesday)',
                DT::dataTableOutput('rst_table')
            )
    ),
    
    # Age Sampling Tab ----
    # tabItem(tabName = 'tab_age',
    #         fluidRow(
    #           column(12,
    #           box(title = 'Age Data Summary', status='info', width= 5,
    #               uiOutput(outputId = 'age_species'),
    #               uiOutput(outputId = 'age_pop_name')
    #               ),
    #           box(width = 7, 
    #               img(src='scale.png', width = '100%', height='auto') 
    #               ))
    #           ),
    #         hr(),
    #         fluidRow(
    #           column(6,
    #                  box(width= 12, fluidRow(column(12, plotlyOutput('n_age_total')))),
    #                  box(width= 12, fluidRow(column(12, plotlyOutput('n_age_ocean')))), 
    #                  box(width= 12, fluidRow(column(12, plotlyOutput('n_age_stream'))))
    #                  ),
    #           column(6,
    #                  box(width= 12, fluidRow(column(12, plotlyOutput('h_age_total')))),
    #                  box(width= 12, fluidRow(column(12, plotlyOutput('h_age_ocean')))),
    #                  box(width= 12, fluidRow(column(12, plotlyOutput('h_age_stream'))))
    #                  )
    #                  )
    #         ),
    
    # Documents Tab ----
    tabItem(tabName = 'tab_documents',
            fluidRow(
              box(width = 12, status = "info",
                  h2('CDMS Document Access', style = 'text-align:center;'),
                  uiOutput(outputId='documentsUI'),
                  DT::dataTableOutput('documents_table')
              ) 
            )
    ),
    
    # Restricted Data Access Tab ----
    # CDMS Datasets ----
    tabItem(tabName = 'tab_cdms',
            box(width = 12, 
                fluidRow(column(6, offset = 3,
                                uiOutput("raw_dataset_menu"),
                                actionButton("raw_submit", label = "Load Selected Dataset", icon = icon('hourglass-start'), width = '100%'),
                )), br()
            ),
            uiOutput('raw_UI')
    ),
    # Custom Queries ----
    tabItem(tabName = 'tab_custom',
            box(width = 12,
                h4('This page is intended to meet the needs of Projects and Biologists to produce desired data views or 
                             summaries.', style = 'text-align: center;'),
                h4('Please contact Tyler Stright (tylers@nezperce.org) with inquiries.', style = 'text-align: center;'),
                hr(),
                fluidRow(
                  column(6, offset = 3,
                         selectInput('custom_query_menu', label = NULL, choices = custom_query_df$query_names, selected = '-Select Custom Query-'),
                         uiOutput('query_description', style = 'text-align:center;'),
                         br(),
                         uiOutput('custom_query_grouping', label = NULL),
                         uiOutput('groupingtext', label ='GRPTXT'),
                         br(),
                         actionButton("custom_submit", label = "Submit Query", icon = icon('hourglass-start'), width = '100%')
                  )
                )
            ),
            uiOutput('custom_UI')
    ),
    # FINS Data ----
    tabItem(tabName = 'tab_fins',
            br(), h1('FINS Data Access', style = 'text-align:center;'), br(),
            finsModuleUI(id='trapping', .title = 'Trapping Module', 
                         .status = 'primary', .choices = c('Facility', 'Trap Year')),
            finsModuleUI(id='spawning', .title = 'Spawning Module', 
                         .status = 'success', .choices = c('Stock', 'Spawn Year'))
    ),
    # Reports ----
    tabItem(tabName = 'tab_reports',
            fluidRow(
              box(width = 12,
                  h2('Reports', style = 'text-align: center;'),
                  br(),
                  h3('This page is itended to be used for automated reports. If you create the same report on a consistent basis (e.g. same graphs and tables of information),
                       we can work together to automate these reports so they are available at the click of a button with the most current data in CDMS.', style='text-align:center;'),
                  h4('Please contact Tyler Stright (tylers@nezperce.org) with inquiries.', style = 'text-align: center;')
              )
            ),
            fluidRow(
              box(width = 12,
                  title = 'Report Downloads',
                  uiOutput('pdf_reports'),
                  # helpText(HTML('<em>*Reports are generated from raw data at the time of request. As such, loading may take several minutes. Clicking the download button multiple times may result in multiple downloads.</em>')),
                  downloadButton('report_export', label = 'Download Adult Report')
              )
            )
    ),
    
    tabItem('tab_contacts',
            br(),
            h1('Contact Information', style = 'text-align:center; color:white; font-family: Arial White;'),
            br(),
            h2('Office Contacts', style = 'color:white'),
            fluidRow(
              column(12,
                     contactInfoUI('JFO'),
                     contactInfoUI('MFO'),
                     contactInfoUI('OFO'),
                     contactInfoUI('SWO'))
            ),
            hr(), 
            h2('Division Contacts', style = 'color:white'),
            fluidRow(
              column(12, 
                     contactInfoUI('Administration'),
                     contactInfoUI('Conservation Enforcement'),
                     contactInfoUI('Harvest'),
                     contactInfoUI('Production'),
                     contactInfoUI('Research'),
                     contactInfoUI('Watershed'))
            ),
            hr(),
            h2('Data Management Contacts', style = 'color:white'),
            fluidRow(
              column(12, 
                     contactInfoUI('DM1'),
                     contactInfoUI('DM2'),
                     contactInfoUI('DM3'),
                     contactInfoUI('DM4'))
            ),
            hr(),
            h2('Kus Web App Contacts', style = 'color:white'),
            fluidRow(
              column(12,
                     contactInfoUI('Kus1'))
            ),
            hr(),
            h2('DFRM Employees', style = 'color:white;'),
            box(width = 12, 
                DT::dataTableOutput('cdms_users'))
    )
    
  ) #tabItems
) #dashboardBody

dashboardPage(title = "Nez Perce Tribe's Kus Web Application", header, sidebar, body, skin= 'blue')
