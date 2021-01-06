windowCountServer <- function(id, data, species_, subtitle_) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$windowCount <- renderValueBox({
        ns <- session$ns # session provides the namespace
        
        n <- data %>%
          filter(species == species_) %>%
          summarise(n = sum(Count, na.rm = TRUE)) %>%
          pull(n)
        
        valueBox(
          value = prettyNum(n, big.mark = ","),
          color = 'aqua',
          icon = icon("fish"),
          subtitle = subtitle_
        )
      })
      
    })
}