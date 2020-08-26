contactInfoUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, uiOutput(ns('position'))),
      column(3, uiOutput(ns('name'))), 
      column(3, uiOutput(ns('phone'))),
      column(3, uiOutput(ns('email')))
    )
  )
}