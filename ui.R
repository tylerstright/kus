#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(title = div(#div(id = "header-id", "KUS: DFRM Fisheries Data"),
                         #div(id = 'username', textOutput('username')),
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
                      div(id='title-text',"Department of Fisheries Resources Management"),
                      uiOutput("home_buttons")
              ),
             navbarMenu("Fish Data",
                        tabPanel(tags$a("Project Data Entry and Validation", href = "https://cdms.nptfisheries.org/index.html#/projects")),
                        tabPanel("Summarized Performance Measures",
                                 h2("This portion of the website is still underconstuction.")),
                        tabPanel("Spawning Ground Survey",
                                 fluidPage(
                                   fluidRow(
                                     column(3,
                                            uiOutput("sgs_dataset_menu")
                                            # radioButtons('sgs_data', h3("Dataset:"), inline = TRUE,
                                            #              choiceNames = c('Redd Data', 'Carcass Data'),
                                            #              choiceValues = c('68', '69'))
                                            ),
                                     column(3,
                                            #checkboxInput(""),
                                            radioButtons('sgs_spp', h3("Species:"),inline = TRUE, choices = c("Chinook salmon", "Steelhead", "Bull trout"))
                                            #checkboxGroupInput('sgs_spp', h3("Species:"),inline = TRUE, choices = c("Chinook salmon", "Steelhead", "Bull trout"))
                                            #selectInput('sgs_spp', h3("Species"), state.name, multiple=TRUE, selectize=FALSE),
                                            ),
                                     column(3,
                                            radioButtons('sgs_run', h3("Run:"), inline = TRUE, choices = c("Spring/summer", "Summer", "Fall"))                                            
                                            ),
                                     column(3,
                                            sliderInput("sgs_year", h3("Survey Year:"), min = 1986, max = year(Sys.Date()),
                                                        step = 1, value = c(1986, year(Sys.Date())), sep='')
                                            #dateRangeInput("sgs_dates", label = h3("Date Range")),                                            
                                            )
                                     ),
                                   fluidRow(
                                      column(4,
                                             uiOutput("mpg_menu")
                                            ),
                                      column(4,
                                            uiOutput("pop_menu")
                                            ),
                                      column(4,
                                            uiOutput("stream_menu")
                                            )#,
                                      # column(3,
                                      #        uiOutput("field_menu")
                                      #        )
                                      ),
                                   fluidRow(
                                     column(6,
                                            helpText("Select at least one Major Population Group, Population, Stream : TributaryTo combination and the field values of interest.")
                                            ),
                                     column(2, offset = 2, align = "center",
                                            actionButton("sgs_submit", label = "Submit Query", class = "mybutton")                                            
                                            ),
                                     # column(2, align = "center",
                                     #        actionButton("sgs_clear", label = "Clear Query", class = "mybutton")
                                     #        ),                                     
                                     column(2, align = "center",
                                            downloadButton("sgs_export", label = "Export .CSV File", class = "mybutton")#,
                                            #tags$head(tags$style(".mybutton{background-color:#333;} .mybutton{color: #333;}"))
                                            )#,
                                     #hr()
                                   ),
                                hr(),
                                 tabsetPanel(
                                   tabPanel("Graphical",
                                            #textOutput("spp_test"),
                                            conditionalPanel('input.sgs_data=="redd_summary"', plotOutput("redd_sum_plot")),
                                            conditionalPanel('input.sgs_data=="redd_detail"', leafletOutput("redd_detail_plot")),
                                            conditionalPanel('input.sgs_data=="carcass_detail', plotOutput("carcass_detail_plot"))
                                            #leafletOutput("redd_detail_plot"),
                                            #plotOutput("redd_sum_plot")

                                  ),
                                   tabPanel("Tabular",
                                            DT::dataTableOutput("sgs_table")
                                            #tableOutput("sgs_table")
                                  )
                                 )
                          )
                        ),
                        tabPanel("Adult Weir",
                                 h2("This portion of the website is still underconstuction.")),
                        tabPanel("Rotary Screw Trap",
                                 h2("This portion of the website is still underconstuction.")),
                        tabPanel("PIT-tag Abundance",
                                 h2("This portion of the website is still underconstuction."))
                        ),
             navbarMenu("Fish Management",
                        tabPanel("Pre- and In-season Predictions",
                                 h2("This portion of the website is still underconstuction.")),
                        tabPanel("Harvest Management",
                                 h2("This portion of the website is still underconstuction."))),
             navbarMenu("Other",
                        tabPanel(tags$a("Hydro-system Operations", href = "https://nptfisheries.shinyapps.io/pitph2/")),
                        tabPanel(tags$a("Imnaha Weir Monitoring", href = "https://nptfisheries.shinyapps.io/PITtrackR/")))
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
