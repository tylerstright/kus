employeeInfoUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('position')), #h3("Administrative Specialist"),
    fluidRow(
      column(4, uiOutput(ns('name'))), #p('Tish Whitman')),
      column(4, uiOutput(ns('phone'))), #p('(208) 843-7320 Ext: 4634')),
      column(4, uiOutput(ns('email'))) #p('tishw@nezperce.org'))
    )
  )
}