employeeInfoServer <- function(id, employee_) { # or should it be position?
  moduleServer(
    id,
    
    function(input, output, session) {
      
      output$position <- renderUI({
        h3(users[match(employee_, users$Name), "Position"]) 
      })
      
      output$name <- renderUI({
        p(users[match(employee_, users$Name), "Name"]) 
      })
      
      output$phone <- renderUI({
        p(users[match(employee_, users$Name), "WorkPhone"], style = 'text-align:center;') 
      })
      
      output$email <- renderUI({
        p(users[match(employee_, users$Name), "Email"], style = 'text-align:right;') 
      })
      
    }
  )
}