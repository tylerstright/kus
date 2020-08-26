contactInfoServer <- function(id, employee_, position_=NULL) { # or should it be position?
  moduleServer(
    id,
    
    function(input, output, session) {
      
      output$position <- renderUI({
        if(is.null(position_)) {
          h4(paste0(users[match(employee_, users$Name), "Position"], ':'), style = 'color:white')
        } else {
          h4(paste0(position_, ':'), style = 'color:white')
        }
        
        # h4(paste0(gsub('[:digit:]', '', id), ':'), style = 'color:white')
      })
      
      output$name <- renderUI({
        h4(users[match(employee_, users$Name), "Name"],  style = 'color:white') 
      })
      
      output$phone <- renderUI({
        h4(users[match(employee_, users$Name), "WorkPhone"], style = 'color:white') 
      })
      
      output$email <- renderUI({
        h4(users[match(employee_, users$Name), "Email"], style = 'color:white') 
      })
      
    }
  )
}