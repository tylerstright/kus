divInfoServer <- function(id, projects_list_) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$select <- renderUI({
        ns <- session$ns # session provides the namespace
        selectInput(ns('select'), 'Select Project', choices = sort(unique(projects_list_$Name)),
                    selected = NULL)
      })
      
      observeEvent(input$select, {
        ProjId <- projects_list[match(input$select, projects_list$Name), 1] # get ProjectId
        proj_info <- getProject(ProjId) # get project summary page info
        proj_meta <- proj_info[["Metadata"]]
        proj_staff <- proj_info[["Editors"]] 
        if(length(proj_staff)==0){NULL}else{
          proj_staff <- proj_staff %>%
            mutate(Description = paste0("(", str_trim(Description), ")"),
                   staff = paste(Fullname, Description))}
        
        output$description <- renderText({
          paste(proj_info[["Description"]])
        })
        
        output$goal <- renderUI({
          goal <- proj_meta[match(9, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')]
          goal <- unlist(str_split(goal, pattern = '\n'))
          HTML(paste(goal, '<br/>', sep=''))
        })
        
        output$objectives <- renderUI({
          objectives <- proj_meta[match(20, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')]
          objectives <- unlist(str_split(objectives, pattern = '\n'))
          HTML(paste(objectives, '<br/>', sep=''))
        })
        
        output$PL <- renderText({
          paste(proj_info[["Owner"]][["Fullname"]], " (",
                proj_meta[match(45, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')], "; ",
                proj_meta[match(44, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')], ")", sep = '')
        })
        
        output$staff <- renderText({
          if(length(proj_staff)==0){paste('NA')}else{paste(proj_staff$staff, sep = ' ', collapse= ', ')}
        })
        
        output$contractors <- renderText({
          paste(gsub(',', ', ', gsub("[^a-zA-Z0-9 ,]", "", proj_meta[match(48, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')])))
        })
        
        output$projectnumber <- renderText({
          paste(proj_meta[match(47, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')])
        })
        
        output$basin <- renderText({
          paste(gsub("[^a-zA-Z0-9 ,]", "",proj_meta[match(7, proj_meta$MetadataPropertyId), which(colnames(proj_meta)=='Values')]))
        })
        
      })
      
      return()
    }#function
  ) #moduleServer
}