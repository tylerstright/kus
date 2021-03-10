finsModuleUI <- function(id, .title, .status, .choices) {
  # id : namespace id to match server function
  # .title : box title for module
  # .status : determines colorbar on box (see ?validStatuses for options)
  # .choices are the radioButton text options to match the .fields in finsModuleServer
  # for the time being, only two .choices should be X and Year, in that order.
  
  ns <- NS(id)
  tagList(
    box(width = 12, title = .title, status = .status, solidHeader = T,
        fluidRow(
          column(6, offset = 3, br(), 
                 br(),
                 radioButtons(inputId = ns('filter_choice'), 
                              label = paste0('Filter data by ' , .choices[[1]], ' or ', .choices[[2]], '?'), 
                              choices = .choices,
                              inline = TRUE, 
                              selected = .choices[[1]]),
                 uiOutput(ns('filter'))
          )
        ),
        fluidRow(
          column(12, align = "center",
                 hr(),
                 downloadButton(ns("export"), label = paste0('Export ', .title, ' .CSV File'))
          )
        )
    )
  )
}
