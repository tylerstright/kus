#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(tidyverse)
library(dplyr)
library(httr)
library(jsonlite)
library(lubridate)
library(plotly)
library(leaflet)
library(shinyjs)
library(viridis)
library(markdown)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(title = div(
                         div(id = 'user-name', uiOutput('login_logout')), # both Log IN and OUT
                         div(id = 'logo-id',img(src="NPTlogos2.png", height = "70px")),
                         tags$a("DFRM Home",href = 'http://www.nptfisheries.org', target = '_blank')

                         ),
             id = "kus_navbar",
             windowTitle = "DFRM-Kus",
             theme = "styles.css",
             position = "fixed-top",
             collapsible = TRUE,
             # footer = div(id = "footer-id",
             #              "The data presented in the Kus web application is not solely collected, managed or owned
             #              by the Nez Perce Tribe. All data should be considered draft and is not guaranteed for
             #              accuracy.  Permission to use the data should be sought from the original collectors."),
             
             tabPanel("Kus Home",
                      useShinyjs(),   # include Shiny JS
                      fluidPage(
                        fluidRow(column(12, align = "center",
                        h2('Department of Fisheries Resources Management'),
                        h3('Data Summaries and Visualizations Web Application'))),
                        fluidRow(
                          column(12, withSpinner(leafletOutput('home_map', height = 690, width = "100%")))
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
                      )
              ),
              navbarMenu("Summarized Data",
                       tabPanel("Snake Basin Population Indicators and Metrics",
                                 sidebarLayout(
                                  sidebarPanel(
                                    style = "position:fixed;width:25%",
                                    width = 3,
                                    h3('Chinook and Steelhead Population Indicators and Metrics'),
                                    helpText('Select species, population and spawn year range of intrest then click "Query Data".'),
                                    uiOutput('pop_spp_menu'),
                                    uiOutput('pop_menu'), 
                                    uiOutput('pop_year_menu'),
                                    actionButton('weir_reset', 'Query Data', class = "mybutton", width = '100%'),
                                    hr(),
                                    leafletOutput('pop_map', height = 400, width = '100%')
                                    ),
                                  mainPanel(
                                    titlePanel('Population Indicators and Metrics'),
                                    fluidPage(
                                      fluidRow(column(12, textOutput('MPGfilter')))
                                    )
                                  )
                                  )
                                ),
                       tabPanel("Adult Weir Returns",
                                sidebarLayout(
                                  sidebarPanel(
                                    style = "position:fixed;width:25%",
                                    width = 3,
                                    h3('Data Collected From Hatchery and Weir Sites'),
                                    helpText('Select species, trap site and spawn year range of intrest then click "Query Data".'),
                                    uiOutput('weir_spp_menu'),
                                    uiOutput('weir_menu'), 
                                    uiOutput('weir_year_menu'),
                                    actionButton("weir_button", "Query Data", class = 'mybutton', width = '100%'),                                    
                                    hr(),
                                    leafletOutput('weir_map', height = 400, width = '100%')
                                  ),
                                  mainPanel(
                                    fluidPage(
                                    #  titlePanel('Weir Returns: Summarized Adult Trapping Data'),
                                     # br(),
                                      fluidRow(column(12, plotOutput('weir_totals', height = 500))),
                                      #),
                                      #hr(),
                                      #titlePanel('Origin and SexSummary Table'),
                                      fluidRow(column(12, withSpinner(DT::dataTableOutput("weir_table"))))
                                      #hr(),
                                      #titlePanel('Disposition Summary Table'),
                                      #fluidRow(column(12, offset = 0, DT::dataTableOutput("weirdisp_table")))
                                    ) 
                                  )
                                )
                                 
                       ),
                       tabPanel("Spawning Ground Surveys",
                               sidebarLayout(
                                   sidebarPanel(
                                     style = "position:fixed;width:25%;",
                                     width = 3,
                                     h3('Data Collected From Spawning Ground Surveys'),
                                     helpText('Select species, streams and spawn year range of intrest then click "Query Data".'),
                                     uiOutput("sgs_species_menu"),
                                     uiOutput("sgs_streams_menu"),                                     
                                     uiOutput("sgs_year_menu"),
                                     actionButton("summ_reset", label = "Query Data", class = "mybutton", width = '100%'),
                                     hr(),
                                     leafletOutput('sgs_map', height = 400, width = '100%')
                                   ),
                                 mainPanel(
                                    fluidPage(
                                      fluidRow(column(12, plotlyOutput(outputId = 'sgs1', height = 400, width = '120%'))),
                                      fluidRow(
                                               splitLayout(cellWidths = c('40%', '40%', '40%'), 
                                                           plotlyOutput(outputId = 'sgs2', height = 300),
                                                           plotlyOutput(outputId = 'sgs3', height = 300, width = '100%'),
                                                           plotlyOutput(outputId = 'sgs4', height = 300 ))
                                             ),                                      
                                      fluidRow(column(12, offset = 0, withSpinner(DT::dataTableOutput("summ_table"))))
                                    )
                                  )
                                )
                              ),
                      tabPanel("Juvenile Abundance and Survival",
                        sidebarLayout(
                          sidebarPanel(
                            style = "position:fixed;width:25%;",
                            width = 3,
                            h3('Data Collected From Rotary Screw Traps and Hatchery Releases'),
                            helpText('Select species, trap site and spawn year range of intrest then click "Query Data".'),
                            uiOutput("juv_species_menu"),
                            uiOutput("juv_streams_menu"),                                     
                            uiOutput("juv_year_menu"),
                            actionButton("juv_reset", label = "Query Data", class = "mybutton", width = '100%'),
                            hr(),
                            leafletOutput('juv_map', height = 400, width = '100%')
                            ),
                          mainPanel(
                            fluidPage(
                               titlePanel('Juvenile Metrics: Abundance and Survival'),
                               br(),
                               fluidRow(
                                        column(6, plotlyOutput('juv_sum1', height = 500)),
                                        column(6, plotlyOutput('juv_sum2', height = 500))                                        ),
                               hr(),
                               fluidRow(column(12, offset = 0, withSpinner(DT::dataTableOutput("rstsumm_table"))))
                            )
                          )
                        )
                      ),
                      tabPanel("Hydro-system Conditions and Fish Counts",
                               sidebarLayout(
                                 sidebarPanel(
                                   style = "position:fixed;width:25%;",
                                   width = 3,
                                   h3('Adult Return Counts and River Conditions at FCRPS Dams'),
                                   h4('Data obtained from Columbia Basin Research and the DART website.'),
                                   helpText('Select the hydrosystem project and return year of intrest then click "Query Data".'),
                                   uiOutput("hydro_menu"),                                     
                                   uiOutput("hydro_year_menu"),
                                   actionButton("hydro_reset", label = "Query Data", class = "mybutton", width = '100%'),
                                   helpText('Change the plot output by selecting species and/or river metric of interest.'),
                                   uiOutput("hydro_species_menu"),
                                   uiOutput("hydro_metric_menu")
                                 ),
                                 mainPanel(
                                   withSpinner(plotOutput("window_plot")),
                                   plotOutput("river_plot")
                                   )
                                 )
                               )
                      ),
             navbarMenu("Fish Management",
                        tabPanel(tags$a("In-season Snake Basin Management", target = '_blank')),
                        tabPanel(tags$a("Hydro-system Operations", href = "https://nptfisheries.shinyapps.io/pitph2/", target = '_blank')),
                        tabPanel(tags$a("PITtrackR", href = "https://nptfisheries.shinyapps.io/PITtrackR/", target = '_blank'))
                        ),
             navbarMenu("Reporting",
                        tabPanel("Juvenile Report", id = 'juv_report', value = 'tab_juvreport',
                                            tags$iframe(style="height:900px; width:100%; scrolling=yes", 
                                                        src="juv_draft1.pdf")#, #in www folder
                                  ),
                        tabPanel("Adult Report", id = 'adult_report', value = 'tab_adultreport',
                                 tags$iframe(style="height:900px; width:100%; scrolling=yes", 
                                                         src="juv_draft1.pdf")) #in www folder)
                       ),
             tabPanel("Raw Data", id = 'raw_data', value = 'tab_rawdata',
                      fluidPage(
                          fluidRow(
                                column(3, uiOutput("raw_dataset_menu"))
                            ),
                          fluidRow(
                            column(6, helpText("Select the desired dataset then the project, stream and locations of interest and click submit query.")),
                            column(2, offset = 2, align = "center",
                                   actionButton("raw_submit", label = "Submit Query", class = "mybutton")),
                            column(2, align = "center",
                                   downloadButton("raw_export", label = "Export .CSV File", class = "mybutton"))
                            )
                        ),
                      hr(),
                      withSpinner(DT::dataTableOutput("raw_table"))
                ),
             navbarMenu("Data Entry", menuName = 'data_entry',
                        tabPanel(tags$a("CDMS - Project Data Entry", href = "https://cdms.nptfisheries.org/index.html#/projects", target = '_blank')),
                        tabPanel(tags$a("LSRCP FINS", href = "https://www.finsnet.org/", target = '_blank'))
                        )
  ) # close navbarPage
) # close shinyUI
