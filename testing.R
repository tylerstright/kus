# 
# 
# source('./R/queryRiverData.R')
# source('./R/queryWindowCnts.R')
library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)
library(manipulateWidget)
# 
# # redd counts
# load('./data/redd_df.rda')
# 
# redd_df <- mutate_at(redd_df, .funs = as.numeric, .vars = c('NewRedds', 'Latitude', 'Longitude')) %>%
#   mutate(SppRun = paste0(Species, " - ", Run))
# 
# # redd Graph
# #-------------------------------------------------------------------------
# all_redds <- redd_df %>%
#   distinct(ActivityId, .keep_all = TRUE) %>%
#   group_by(ESU, MPG, POP, SppRun, SurveyYear) %>%
#   summarise(TotalRedds = sum(NewRedds, na.rm = TRUE))
# # Fall Chinook
# fach_redds <- redd_df %>%
#   distinct(ActivityId, .keep_all = TRUE) %>%
#   group_by(ESU, MPG, POP, SppRun, SurveyYear) %>%
#   filter(SppRun == "Chinook salmon - Fall") %>%
#   summarise(TotalRedds = sum(NewRedds, na.rm = TRUE))
# # Steelhead
# steel_redds <- redd_df %>%
#   distinct(ActivityId, .keep_all = TRUE) %>%
#   group_by(ESU, MPG, POP, SppRun, SurveyYear) %>%
#   filter(SppRun == "Steelhead - Summer") %>%
#   summarise(TotalRedds = sum(NewRedds, na.rm = TRUE))
# # Spring Chinook
# spch_redds <- redd_df %>%
#   distinct(ActivityId, .keep_all = TRUE) %>%
#   group_by(ESU, MPG, POP, SppRun, SurveyYear) %>%
#   filter(SppRun == "Chinook salmon - Spring/summer") %>%
#   summarise(TotalRedds = sum(NewRedds, na.rm = TRUE))
# 
# Plot Function
# reddPlot <- function(RbySpc) {
#     species_values <- c("Steelhead - Summer", "Chinook salmon - Fall", "Chinook salmon - Spring/summer") #unlist(unique(all_redds$SppRun))
#     if(RbySpc == "Steelhead - Summer") {
#          plot_ly(steel_redds,
#                 x = ~SurveyYear,
#                 y = ~TotalRedds,
#                 type = 'scatter',
#                 mode = 'lines+markers',
#                 color = ~as.factor(`POP`))%>%
#         layout(legend = list(x = 100, y = 0.5),
#                title = "Summer Steelhead",
#                xaxis = list(title = 'Survey Year',
#                             zeroline = TRUE),
#                yaxis = list(title = 'Total Number of Redds')
#         )
#     } else {
#             if(RbySpc == "Chinook salmon - Fall") {
#                       plot_ly(fach_redds,
#                          x = ~SurveyYear,
#                          y = ~TotalRedds,
#                          type = 'scatter',
#                          mode = 'lines+markers',
#                          color = ~as.factor(`POP`)) %>%
#                 layout(legend = list(x = 100, y = 0.5),
#                        title = "Fall Chinook",
#                        xaxis = list(title = 'Survey Year',
#                                     zeroline = TRUE),
#                        yaxis = list(title = 'Total Number of Redds')
#                 )
#             } else {
#                 if(RbySpc == "Chinook salmon - Spring/summer") {
#                         plot_ly(spch_redds,
#                             x = ~SurveyYear,
#                             y = ~TotalRedds,
#                             type = 'scatter',
#                             mode = 'lines+markers',
#                             color = ~as.factor(`POP`)) %>%
#                     layout(legend = list(x = 100, y = 0.5),
#                            title = "Spring Chinook",
#                            xaxis = list(title = 'Survey Year',
#                                         zeroline = TRUE),
#                            yaxis = list(title = 'Total Number of Redds')
#                     )
# 
#                 }
# 
#             }
#     }
# }
# 
# 
# manipulateWidget(
#   reddPlot(RbySpc),
#   RbySpc = mwSelect(choices = species_values)  #??
# )
# 
# #//////////////////////////////////////////////////////////////  
# # a <- plot_ly(fach_redds, 
# #              x = ~SurveyYear, 
# #              y = ~TotalRedds, 
# #              type = 'scatter', 
# #              mode = 'lines+markers', 
# #              color = ~as.factor(`POP`)) %>%
# #   layout(legend = list(x = 100, y = 0.5),
# #          title = "Fall Chinook",
# #          xaxis = list(title = 'Survey Year',
# #                       zeroline = TRUE),
# #          yaxis = list(title = 'Total Number of Redds')
# #   ) 
# # 
# #   b <- plot_ly(spch_redds, 
# #                x = ~SurveyYear, 
# #                y = ~TotalRedds, 
# #                type = 'scatter', 
# #                mode = 'lines+markers',
# #                color = ~as.factor(`POP`)) %>%
# #     layout(legend = list(x = 100, y = 0.5),
# #            title = "Spring Chinook",
# #            xaxis = list(title = 'Survey Year',
# #                    zeroline = TRUE),
# #            yaxis = list(title = 'Total Number of Redds')
# #     )
# #   c <- plot_ly(steel_redds, 
# #                x= ~SurveyYear, 
# #                y= ~TotalRedds, 
# #                type= 'scatter', 
# #                mode= 'lines+markers',
# #                color = ~as.factor(`POP`)) %>%
# #     layout(legend = list(x = 100, y = 0.5),
# #            title = "Summer Steelhead",
# #            xaxis = list(title = 'Survey Year',
# #                         zeroline = TRUE),
# #            yaxis = list(title = 'Total Number of Redds')
# #     )
# # 
# # abc <- subplot(a, b, c, nrows= 3, shareX = TRUE, shareY = FALSE) 
# #   
# # 
# # print(a)
# # print(b)
# # print(c)    # This could be way more clean I believe.  
# # print(abc)  # Need to further investigate facet_wrap for plot_ly
# #   
#   
# 
#   
# #----------------------------------------  
# # window counts
# win_df2 <- bind_rows(queryWindowCnts(dam = 'LWG', spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
#                                     spawn_yr = year(Sys.Date()), start_day = '01/01', end_day = format(Sys.Date(), '%m/%d')) %>%
#                       mutate(Site = 'LWG'),
#                     queryWindowCnts(dam = 'BON', spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
#                                     spawn_yr = year(Sys.Date()), start_day = '01/01', end_day = format(Sys.Date(), '%m/%d')) %>%
#                       mutate(Site = 'BON')) %>%
#   mutate(Chinook = Chinook + Jack_Chinook,
#          Coho = Coho + Jack_Coho,
#          Dam = ifelse(Site == 'LWG', 'Lower Granite', 'Bonneville'),
#          Date = as.Date(Date)) %>%
#   select(Site, Dam, Date, Chinook, Coho, Steelhead, Wild_Steelhead, Lamprey) %>%
#   gather(species, count, Chinook:Lamprey)
#   
# 
# damcount <- renderPlotly({
#   p <- win_df %>%
#     filter(Dam == 'Bonneville') %>%
#     gather(species, count, Chinook:Lamprey) %>%
#     ggplot(aes(x = Date, y = count)) +
#     geom_bar(aes(fill = species), stat = 'identity') +
#     scale_fill_viridis_d() +
#     scale_x_date(date_labels = format('%b-%d')) +
#     facet_grid(species~Dam, scales = 'free_y') +
#     theme_grey() +
#     theme(legend.position = 'none') +
#     labs(x = 'Date',
#          y = 'Daily Window Count',
#          title = ''
#     )
#   ggplotly(damcount) %>% layout(margin = list(b = 50, l = 90))
# })
# print(damcount)
# 
# AAA <-plot_ly(win_df2,
#         x = ~Date,
#         y = ~count,
#         type = 'bar',
#         colors = ~as.factor(species))
# print(AAA)
# # my graph
# ty_damcount <- function(`Species:`, `Dam:`) {
#   var_win_df2 <- win_df2 %>%
#     filter(species == `Species:`,
#            Dam == `Dam:`)
#     plot_ly(var_win_df2,
#             x = ~Date,
#             y = ~count,
#             type = 'bar',
#             colors = 'gray') %>%
#       layout(title = paste0(`Dam:`, " Dam Daily Window Counts: ", `Species:`),
#              font = list(
#                family = 'Arial Narrow',
#                size = 14,
#                color = 'red')) %>%
#       layout(legend = list(x = 100, y = 0.5),
#              # title = paste0(`Dam:`, " Dam Daily Window Counts: ", `Species:`),
#              xaxis = list(title = 'Date',
#                           color = 'blue',
#                           zeroline = TRUE),
#              yaxis = list(title = 'Count',
#                           color = 'black')
#       )
# }
# 
# # these create lists for dropdown widgets
# fishes <- c("Chinook", "Coho", "Steelhead", "Wild_Steelhead", "Lamprey")
# dams <- c("Lower Granite", "Bonneville")
# 
# manipulateWidget(
#   ty_damcount(`Species:`, `Dam:`),
#   `Species:` = mwSelect(choices = fishes),
#   `Dam:` = mwSelect(choices = dams)
# )
# 
# #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# # window count graphs
# 
# # river flows
# river_df <- bind_rows(queryRiverData(site = 'LWG',
#                                      year = year(Sys.Date()),
#                                      start_day = '01/01',
#                                      end_day = format(Sys.Date(), '%m/%d')) %>%
#                         mutate_all(as.character),
#                       queryRiverData(site = 'BON',
#                                      year = year(Sys.Date()),
#                                      start_day = '01/01',
#                                      end_day = format(Sys.Date(), '%m/%d')) %>%
#                         mutate_all(as.character)) %>%
#   mutate(Dam = ifelse(Site == 'LWG', 'Lower Granite', 'Bonneville'),
#          Date = as.Date(Date),
#          Inflow = as.numeric(Inflow)) %>%
#   
#   select(Dam, Site, Date, everything())
# 
# # river flow graphs
# 
# 
# 
# 
# 
# 
# 

#==============================================================================
# SERVER/UI
ui <- fluidPage( id = 'fluidpage',
  useShinyjs(),
  tabsetPanel(
  tabPanel('tab 1',
    textInput('text', "text", NULL),
    actionButton('button', 'button'),
    textOutput('response'),
    textOutput('print_text'),
    textOutput('print_total'),
    textOutput('print_X'),
    textOutput('TEST')
  ),
  tabPanel(title = "Data Access", value = 'hiddentab',
           fluidPage(
             fluidRow(
               column(3, "dataset_menu"),
               column(3, "project_menu"),
               column(3, "waterbody_menu"),
               column(3, "location_menu")
             )
           ))
  # tabPanel("Data Access",
  #          #actionButton('button', 'button'),
  #          "Login required for Data Access.",
  #         uiOutput('vartab'))
  ),
  fixedPanel(
    selectInput('selinputect', 'Choose One:', c('blue', 'red'))
  )
)



server <- function(input, output) {

  observeEvent(input$button, {
    showModal(modalDialog(
              textInput('text2', 'text2'),
              actionButton('button2', 'button2'))
    )
  })

  observeEvent(input$button2, {
    if(input$text2 == 'reveal') {
      toggle(selector = "#fluidpage li a[data-value=hiddentab]")
        removeModal()
    }
  })

  observe({
    hide(selector = "#fluidpage li a[data-value=hiddentab]" )  # start & hide tab (shows for tincy bit)
  })

  # observeEvent(input$button, {
  #     toggle(selector = "#fluidpage li a[data-value=hiddentab]")   # functional
  #   })


  # observeEvent(input$button, {
  #   if(input$text == 'access') {
  #   toggle(selector = "#fluidpage li a[data-value=hiddentab]")
  #   }
  # })


  # create reactive UI output
  # observeEvent(input$button, {
  #   if(input$text == 'access') {
  #     output$vartab <- renderUI({
  #       tabPanel("Data Access",
  #                fluidPage(
  #                   fluidRow(
  #                     column(3, "dataset_menu"),
  #                     column(3, "project_menu"),
  #                     column(3, "waterbody_menu"),
  #                     column(3, "location_menu")
  #                   )
  #         ))
  #     })
  #   } else {
  #      NULL
  #    }
  # })



 # different render response based on text input
  observeEvent(input$button, {
    if(input$text != 'access') {
      output$response <- renderText({
                  paste0("negative")
                  })
    } else {
        if(input$text == 'access') {
          output$response <- renderText({
                      paste0("access granted")
                      })
        }
    }
    })


  observeEvent(input$button, {
    if(value$total < 5) {
      output$TEST <- renderText({
        paste0("Less than 5")
      })
    } else {
      output$TEST <- renderText({paste0("5 or more")
    })
    }
  })

  # clicking button creates addative value.
  value <- reactiveValues(total = 0)

  observeEvent(input$button, {
    value$total <- value$total+1
  })

  output$print_total <- renderText({
    paste0(value$total)
  })

  # see if this updates
  value2 <- reactiveValues(total2 =100)

  observeEvent(value$total, {
    value2$total2 <- value2$total2 +1
  })

  output$print_X <- renderText({
    paste0(value2$total2)
  })

  # makeReactiveBinding(X)
  # X <- paste0(input$text)
  #
  # prints out input$username
  output$print_text <- renderText({
    paste0("Text: ", input$text)
  })


}

shinyApp(ui = ui, server = server)
