contactInfoServer <- function(id, employee_, position_=NULL) { # or should it be position?
  moduleServer(
    id,
    
    function(input, output, session) {
      # position will auto fill from users table unless specified in argument
      output$position <- renderUI({
        if(is.null(position_)) {
          h4(paste0(users[match(employee_, users$Name), "Position"], ':'), style = 'color:white')
        } else {
          h4(paste0(position_, ':'), style = 'color:white')
        }
      })
      
      output$name <- renderUI({
        h4(users[match(employee_, users$Name), "Name"],  style = 'color:white') 
      })
      
      output$phone <- renderUI({
        if(is.na(users[match(employee_, users$Name), "Extension"]) |
           str_extract(users[match(employee_, users$Name), "WorkPhone"], '[:digit:]{4}$') == users[match(employee_, users$Name), "Extension"]) {
          h4(users[match(employee_, users$Name), "WorkPhone"], style = 'color:white') 
        } else {
          h4(paste(users[match(employee_, users$Name), "WorkPhone"], 'Ext:',
                   users[match(employee_, users$Name), "Extension"]), style = 'color:white') 
        }
      })
      
      output$email <- renderUI({
        h4(users[match(employee_, users$Name), "Email"], style = 'color:white') 
      })
      
    }
  )
}
