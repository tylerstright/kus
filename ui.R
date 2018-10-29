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
library(lubridate)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(title = div(#div(id = "header-id", "KUS: DFRM Fisheries Data"),
                         div(id = 'user-name', textOutput('username')),
                         div(id = 'logo-id',img(src="NPTlogos2.png", height = "70px")),
                         tags$a("DFRM Home",href = 'http://www.nptfisheries.org')

                         ),
             id = "kus_navbar",
             windowTitle = "DFRM-Kus",
             theme = "styles.css",
             position = "fixed-top",
             collapsible = TRUE,
             footer = div(id = "footer-id",
                          "The data presented in the Kus web application is not solely collected, managed or owned
                          by the Nez Perce Tribe. All data should be considered draft and is not guaranteed for
                          accuracy.  Permission to use the data should be sought from the original collectors and managers
                          which can be found on the About Us tab."),
             
             tabPanel("Kus Home",
                      div(id='home-photo'),
                      div(class='homecontainer',
                        div(id = 'home-title',"Department of Fisheries Resources Management")
                        #div(class = 'homebuttons',
                        #    div(class = 'homebutton', style="display: inline_block", actionButton(inputId = 'sum_tab', label = "Performance Measures"))
                        #  div(class = 'homebutton', style="display: inline-block;", "Raw Data"),
                        #  div(class = 'homebutton', style="display: inline-block;","Data Entry"),
                        #  div(class = 'homebutton', style="display: inline-block;", "Summarized Data"),
                        #  div(class = 'homebutton', style="display: inline-block;", "Other Applications")
                        #  )
                        #,uiOutput("home_buttons")
                        )
              ),
             navbarMenu("Fish Management",
                        tabPanel("Pre- and In-season Management",
                                 h2("This portion of the website is still under constuction.")),
                        tabPanel(tags$a("Hydro-system Operations", href = "https://nptfisheries.shinyapps.io/pitph2/")),
                        tabPanel(tags$a("Imnaha Weir Monitoring", href = "https://nptfisheries.shinyapps.io/PITtrackR/"))
                        ),
             tabPanel("Summarized Data", id = 'summarized_data',
                      fluidPage(
                        fluidRow(
                          column(3, uiOutput("sum_dataset_menu"))
                        ),
                        fluidRow(
                          column(6, helpText("Select the desired dataset you with to view and click submit query.")),
                          column(2, offset = 2, align = "center",
                                 actionButton("sum_submit", label = "Submit Query", class = "mybutton")),
                          column(2, align = "center",
                                 downloadButton("sum_export", label = "Export .CSV File", class = "mybutton"))
                        )
                      ),
                      hr(),
                      tabsetPanel(
                        tabPanel("Graphical",
                                 plotOutput("sum_plot"),
                                 conditionalPanel('input.data=="redd_summary"', plotOutput("redd_sum_plot")),
                                 conditionalPanel('input.data=="redd_detail"', leafletOutput("redd_detail_plot")),
                                 conditionalPanel('input.data=="carcass_detail', plotOutput("carcass_detail_plot"))
                                 #leafletOutput("redd_detail_plot"),
                          ),
                        tabPanel("Tabular",
                                 withSpinner(DT::dataTableOutput("sum_table"))
                          )
                      )
              ),
             tabPanel("Raw Data", id = 'raw_data',
                      fluidPage(
                          fluidRow(
                                column(3, uiOutput("dataset_menu")),
                                column(3, uiOutput("project_menu")),
                                column(3, uiOutput("waterbody_menu")),
                                column(3, uiOutput("location_menu"))
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
              # tabPanel("About Us",
              #          fluidPage(
              #            fluidRow(
              #              column(12,
              #                     h3("Contacts"),
              #                     align = "center"
              #                     )
              #            ),
              #            hr(),
              #            fluidRow(
              #              column(3,
              #                     h4("Ryan Kinzer"),
              #                     h5("Research Scientist"),
              #                     h5("McCall Field Office"),
              #                     h5("208-634-5290"),
              #                     h5("ryank@nezperce.org"),
              #                     align = "center"
              #                     ),
              #              column(3,
              #                     h4("Clark Watry"),
              #                     h5("Data Steward"),
              #                     h5("Sweetwater Field Office"),
              #                     h5("208-843-9184"),
              #                     h5("clarkw@nezperce.org"),
              #                     align = "center"
              #                     ),
              #              column(3,
              #                     h4("Tyler Stright"),
              #                     h5("Data Management Biologist"),
              #                     h5("McCall Field Office"),
              #                     h5("208-634-5290"),
              #                     h5("tylers@nezperce.org"),
              #                     align = "center"
              #              ),
              #              column(3,
              #                     h4("Samantha Smith"),
              #                     h5("Data Techinician III"),
              #                     h5("Sweetwater Field Office"),
              #                     h5("208-843-9184"),
              #                     h5("samanthas@nezperce.org"),
              #                     align = "center"
              #              )
              #            ),
              #            hr(),
              #            fluidRow(
              #              column(12,
              #                     h3("Acknowledgements"),
              #                     align = "center"
              #              )
              #            ),
              #            fluidRow(
              #              column(12,
              #                     p("The data presented in the Kus web application is collected and managed by
              #                       multiple tribes, states and federal agencies. Without their permission and
              #                       generous policies regarding public and regional data sharing, Kus would not
              #                       be possible. We would like to individually thank all of people involved with
              #                       the data presented on this site, however, we are afraid the list of folks is
              #                       too numerous. If we have used data that you are personally connected with
              #                       we would like to say, thank you!  And if you feel any data presented here
              #                       is portrayed incorrectly, or you would like removed from the application
              #                       please contact us to discuss."))
              #            ),
              #            hr(),
              #            fluidRow(
              #              column(12,
              #                     h3("Data Sources and References"),
              #                     align = "center"
              #              )
              #            ),
              #            hr(),
              #            fluidRow(
              #              column(12,
              #                     h4("Hydro-system Operation and River Conditions"),
              #                     h5("Columbia Basin Research and the Data in Real-Time (DART) website"))
              #            )
              #          )
              #      )
  ) # close navbarPage
) # close shinyUI
