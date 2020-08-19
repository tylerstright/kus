divInfoUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = 'Project Information', solidHeader = TRUE, width = 12, status = 'info',
        uiOutput(ns('select')),
        h3("Description"),
        uiOutput(ns('description')), br(),
        h3('Goal Statement'),
        htmlOutput(ns('goal')), br(),
        h3('Objectives'),
        htmlOutput(ns('objectives')), br(),
        h3('Project Leader'),
        uiOutput(ns('PL')), br(),
        h3('Staff'),
        uiOutput(ns('staff')), br(),
        h3('Contracting Agencies'),
        uiOutput(ns('contractors')), br(),
        h3('Project Number(s)'),
        uiOutput(ns('projectnumber')), br(),
        h3('Basin/Sub-basin'),
        uiOutput(ns('basin'))
    ),
    box(title = 'Project Pictures', solidHeader = TRUE, width = 4, status = 'info',
        column(12, class = 'w3-container', 
               uiOutput(ns('picture'))
        )
    )
  )
}