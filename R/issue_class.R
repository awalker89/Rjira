
## Not sure about this

## How this could work
# we have an issue object
# eveytime you change something on the issue object a request is sent to change OR you do what you want and then send it at the send it at the end





Issue <- setRefClass(
  
  Class = "Issue",
  
  fields = list(
    
    project_key = "ANY"
    , project_ID = "ANY"
    , issue_type = "ANY"
    , summary = "ANY"
    , assignee = "ANY"
    , description = "ANY"    
    
  )
  
  ,methods = list(
    
    initialize = function(){
      
    }
    
  )
  
)

Issue$methods(to_project_list = function(){
  
  
  if(!is.null(project_key)){
    project_part <- c(key = project_key)
  }else if(!is.null(project_id)){
    project_part <- c(ID = project_id)
  }else{
    stop("project_id and project_key are both NULL")
  }
  
  
  list(
    fields = list(
      project = project_part,
      summary = summary,
      description = description,
      issuetype = c(name = issue_type)
    )
  )
  
  
  
})






# Issue$methods(show = function(){
#   
#   print(project)
#   
# })


