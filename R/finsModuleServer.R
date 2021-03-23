finsModuleServer <- function(id, .data, .choices, .fields) {
  # id : namespace to match UI function
  # .data : FINS data from specified module (e.g. spawning, trapping, etc.)
  # .choices : exact match of the .choices in finsModuleUI 
  # .fields : the exact field names in .data to match the .choices values. !!These are not always identical!!

  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$filter_choice, {
        if(input$filter_choice == .choices[[1]]) { # non-year filter.
          output$filter <- renderUI({
            ns <- session$ns
            tagList(
              selectInput(inputId = ns('filter'),
                          label = paste0('Select ', .choices[[1]], ' *'),
                          choices = sort(unique(.data[[.fields[[1]]]])),
                          selected = year(Sys.Date())),
              h3(paste0('*This query will return all years of data for the chosen ', tolower(.choices[[1]]), '.'), style='text-align:center;')
            )
          })
        }
        else {
          output$filter <- renderUI({
            ns <- session$ns 
            tagList(
              selectInput(inputId = ns('filter'),
                          label = paste0('Select ', .choices[[1]], ' *'),
                          choices = sort(unique(.data[[.fields[[2]]]])), 
                          selected = NULL),
              h3(paste0('*This query will return a single ', tolower(.choices[[2]]), ' of data containing every ', tolower(.choices[[1]]), '.'), style='text-align:center;')
            )
          })
        }
      })
      
      # Export 
      output$export <- downloadHandler(
        filename = function() {
          paste('FINS_', id, '_', gsub(' ','_',input$filter), '_', Sys.Date(), ".csv", sep='')
        },
        content = function(file) {
          write.csv(.data %>% 
                      # apply filter
                      filter(if(input$filter_choice == .choices[[1]]) .data[[.fields[[1]]]] == input$filter else .data[[.fields[[2]]]] == input$filter),
                    file, row.names = FALSE, na='')
        },
        contentType = "text/csv"
      )
      
    }#function
  ) #moduleServer
}