summarySelectUI <- function(id, .title, .status, .img_path) {
  # id : namespace id to match server function
  # .title : box title for summary page
  # .status : determines colorbar on box (see ?validStatuses for options)
  # .img_path : name of to desired image in the www folder
  
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             box(title = .title, status = .status, solidHeader = T, width = 5, 
                 uiOutput(ns('select1')),
                 uiOutput(ns('select2')),
                 uiOutput(ns('select3'))
             ),
             box(width = 7, 
                 img(src=.img_path, width='100%', height='auto')
             )
      ) # column
    ) # fluidRow
  ) # tagList
} # function
