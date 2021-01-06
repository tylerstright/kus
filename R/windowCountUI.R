windowCountUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(3,
           valueBoxOutput(ns("windowCount"), width = NULL)
    )
  )
}