





#' @name create_issue
#' @title Create an Issue object
#' @return Issue object
#' @export
#' @examples
#' create_issue()
create_issue <- function(){
  return(Issue$new())
}




Issue <- setRefClass(
  
  Class = "Issue",
  
  fields = list(
    
    project_key = "ANY"
    , project_ID = "ANY"
    , issue_type = "character"
    , summary = "ANY"
    , assignee = "ANY"
    , description = "ANY"    
    
  )
  
  ,methods = list(
    
    initialize = function(){
     
      project_key <<- NULL
      project_ID <<- NULL
      issue_type <<- "Task"
      summary <<- NULL
      assignee <<- NULL
      description <<- NULL   
       
    }
    
  )
  
)


Issue$methods(to_issue_list = function(){
  
  
  if(!is.null(project_key)){
    project_part <- c(key = project_key)
  }else if(!is.null(project_id)){
    project_part <- c(ID = project_id)
  }else{
    stop("project_id and project_key are both NULL")
  }

  
  fields <- list()
  fields$project <- project_part
  fields$summary <- summary
  
  if(!is.null(description))
    fields$description <- description
  
  if(!is.null(assignee))
    fields$assignee <- c(name = assignee)
  
  fields$issuetype <- c(name = issue_type)
  
  return(list("fields" = fields))
  
  
})



# Issue$methods(show = function(){
#   
#   print(project)
#   
# })


