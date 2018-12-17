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
library(httr)
library(lubridate)
library(plotly)
library(leaflet)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(title = div(div(id = 'user-name', textOutput('full_name')),
                         div(id = 'user-name', uiOutput('log_link')),
                         div(id = 'logo-id',img(src="NPTlogos2.png", height = "70px")),
                         tags$a("DFRM Home",href = 'http://www.nptfisheries.org')

                         ),
             id = "kus_navbar",
             windowTitle = "DFRM-Kus",
             theme = "styles.css",
             position = "fixed-top",
             collapsible = TRUE,
             # footer = div(id = "footer-id",
             #              "The data presented in the Kus web application is not solely collected, managed or owned
             #              by the Nez Perce Tribe. All data should be considered draft and is not guaranteed for
             #              accuracy.  Permission to use the data should be sought from the original collectors and managers
             #              which can be found on the About Us tab."),
             
             tabPanel("Kus Home",
                      useShinyjs(),   # include Shiny JS
                      fluidPage(
                        fluidRow(
                          column(12, leafletOutput('redd_map', height = 500, width = "100%"))
                        ),
                        fluidRow(
                          column(12, withSpinner(plotlyOutput('home_river')))
                        ),
                        hr(),
                        fluidRow(
                          column(6, withSpinner(plotlyOutput('home_BONwin', height = 800))),
                          column(6, withSpinner(plotlyOutput('home_LGRwin', height = 800)))
                        ),
                        hr(),
                        fluidRow(
                          column(12, withSpinner(plotlyOutput('home_redd')))
                        )
                      )
              ),
             navbarMenu("Summarized Data",
                      tabPanel("Snake Basin Populaiton Indicators and Metrics"),
                      tabPanel("Spawning Ground Surveys"),
                      tabPanel("Weir Returns"),
                      tabPanel("Hydro-system Conditions and Fish Counts",
                               uiOutput("year_menu"),
                               actionButton("year_submit", "Query River Data", class = 'mybutton'),
                               withSpinner(plotOutput("river_plot")),
                               uiOutput("river_menu"),
                               withSpinner(plotOutput("window_plot")),
                               uiOutput("spp_menu")
                               )
             ),
             navbarMenu("Fish Management",
                        tabPanel(tags$a("In-season Snake Basin Management")),
                        tabPanel(tags$a("Hydro-system Operations", href = "https://nptfisheries.shinyapps.io/pitph2/")),
                        tabPanel(tags$a("PITtrackR", href = "https://nptfisheries.shinyapps.io/PITtrackR/"))
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
                        tabPanel(tags$a("CDMS - Project Data Entry", href = "https://cdms.nptfisheries.org/index.html#/projects")),
                        tabPanel(tags$a("LSRCP FINS", href = "https://www.finsnet.org/"))
                        )
  ) # close navbarPage
) # close shinyUI
