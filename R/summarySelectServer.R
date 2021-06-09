summarySelectServer <- function(id, .data, .select_count = c(1:3), .field_names,
                                .field_labels = NULL,
                                .radio = FALSE) {
  # id : namespace to match UI function
  # .data : FINS data from specified module (e.g. spawning, trapping, etc.)
  # .select_count : number of inputs desired for summary page.
  # .field_names : the desired field names in .data for filtering
  # .field_labels : if the field aren't what you want to display, enter labels.
  # .radio : If TRUE, select1 will be radio buttons instead of a selectInput
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # select1 ----
      select1_var <- .field_names[1]
      select1_label <- if_else(is.null(.field_labels), 
                                       str_to_title(gsub('_', ' ', .field_names[1])),
                                       .field_labels[1])
      
      if(.radio==FALSE) {
        # Note: select1_df == .data
        output$select1 <- renderUI({
          ns <- session$ns  # include NS
          selectInput(inputId= ns('select1'), 
                      label= paste0('Choose ',  select1_label, ':'), 
                      choices= sort(unique(.data[[select1_var]])),
                      selected = names(sort(table(.data[[select1_var]]), decreasing=TRUE)[1]),
                      # start with nothing selected: character(0)
                      selectize= FALSE,
                      multiple = FALSE)
        })
      } else {
        output$select1 <- renderUI({
          ns <- session$ns 
          radioButtons(inputId= ns('select1'), 
                       label = paste0('Choose ',  select1_label, ':'), 
                       inline=TRUE,
                       choices = sort(unique(.data[[select1_var]])), 
                       selected = names(sort(table(.data[[select1_var]]), decreasing=TRUE)[1]))
        })
      }
      
      # select2 ----
      if(.select_count %in% c(2,3)) {
        select2_df <- reactive({.data[.data[select1_var]==input$select1, ]})
        select2_var <- .field_names[2]
        select2_label <- if_else(is.null(.field_labels), 
                                         str_to_title(gsub('_', ' ', .field_names[2])),
                                         .field_labels[2])
        
        output$select2 <- renderUI({
          ns <- session$ns
          selectInput(inputId= ns('select2'), 
                      label= paste0('Choose ',  select2_label, ':'), 
                      choices= sort(unique(select2_df()[[select2_var]])),
                      selected = names(sort(table(select2_df()[[select2_var]]), decreasing=TRUE)[1]),
                      selectize= FALSE,
                      multiple = FALSE)
        })
      }
      
      # select3 ----
      if(.select_count == 3) {
        select3_df <- reactive({select2_df()[select2_df()[select2_var]==input$select2, ]})
        select3_var <- .field_names[3]
        select3_label <- if_else(is.null(.field_labels), 
                                         str_to_title(gsub('_', ' ', .field_names[3])),
                                         .field_labels[3])
        
        output$select3 <- renderUI({
          ns <- session$ns
          selectInput(inputId= ns('select3'), 
                      label= paste0('Choose ',  select3_label, ':'), 
                      choices= sort(unique(select3_df()[[select3_var]]), decreasing=TRUE), 
                      selected = names(sort(table(select3_df()[[select3_var]]), decreasing=TRUE)[1]),
                      selectize= FALSE,
                      multiple = FALSE)
        })
      }
      
      # reactive dataframe output
      if(.select_count==1){
        filtered_df <- reactive({.data[.data[select1_var]==input$select1, ]})
      }
      if(.select_count==2){
        filtered_df <- reactive({select2_df()[select2_df()[select2_var]==input$select2, ]})
      }
      if(.select_count==3){
        filtered_df <- reactive({select3_df()[select3_df()[select3_var]==input$select3, ]})
      }
      
      return(filtered_df) # reactive dataframe.
    }#function
  ) #moduleServer
}
