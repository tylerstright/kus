# Login authentication

# package 'secret' may be used to secure db connection pwd

library(RODBC)
channel <- odbcConnect("joy_test_sql_data_source", uid="shiny_test", pwd="shiny123")

login_table<<-as.data.frame(sqlQuery(channel,"select * FROM [R_shiny_test].[dbo].[login_id]"))

observeEvent(input$login,{
  
  uid_t<-isolate(input$uid)
  pwd_t<-isolate(input$password)
  if(input$uid=="")
  {
    showModal(modalDialog(
      title = "Invalid",
      "Please Fill Username"
    ))
  }
  else if(input$password=="")
  {
    showModal(modalDialog(
      title = "Invalid",
      "Please Fill Password"
    ))
  }
  else if(input$uid=="" &&input$password=="" )
  {
    showModal(modalDialog(
      title = "Invalid",
      "Please Fill Username & Password"
    )) 
  }
  else if(ui_t %in% login_table$username==TRUE|pwd_t %in% login_table$password==TRUE)
  {
    temp_login<-login_table[(login_table$username == uid_t ), ]
    if(temp_login$username==input$uid && temp_login$password==input$password)
    {
      library(tcltk)
      tkmessageBox(title = "XyBot",message = "Login Sucessful", icon = "info", type = "ok")
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      updateTabsetPanel(session, "tabs",selected ="data_upload")
      user_logged<-1
      shinyjs::disable("login_box")
      
    }
    else
    {
      tkmessageBox(title = "XyBot",message = "Wrong Credentials", icon = "info", type = "ok")
    }
  }
  else 
  {
    showModal(modalDialog(
      title = "Wrong",
      "Please Check your Credentials "
    ))
  }
  
})