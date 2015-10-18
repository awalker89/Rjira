





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
    
    project_key = "character"
    , project_ID = "integer"
    , issue_type = "character"
    , summary = "character"
    , assignee = "character"
    , reporter = "character"
    , description = "character"
    , duedate = "Date"
    
  )
  
  ,methods = list(
    
    initialize = function(){
     
      project_key <<- as.character(NA)
      project_ID <<- as.character(NA)
      issue_type <<- "Task"
      summary <<- as.character(NA)
      assignee <<- as.character(NA)
      reporter <<- as.character(NA)
      description <<- as.character(NA)
      duedate <<- as.Date(NA)   
      
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
  fields$issuetype <- c(name = issue_type)
  
  if(!is.na(description))
    fields$description <- description
  
  if(!is.na(assignee))
    fields$assignee <- c(name = assignee)
  
  if(!is.na(reporter))
    fields$reporter <- c(name = reporter)
  
  if(!is.na(duedate))
    fields$duedate <- format(duedate, "%Y-%m-%d")

  
  return(list("fields" = fields))
  
  
})



# Issue$methods(show = function(){
#   
#   print(project)
#   
# })


