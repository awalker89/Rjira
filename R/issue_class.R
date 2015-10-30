





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
    , description = "ANY"
    , duedate = "Date"
    , components = "character"
    , custom_fields = "list"
    
  )
  
  ,methods = list(
    
    initialize = function(){
     
      project_key <<- as.character(NA)
      project_ID <<- as.integer(NA)
      issue_type <<- "Task"
      summary <<- as.character(NA)
      assignee <<- as.character(NA)
      reporter <<- as.character(NA)
      description <<- NULL
      duedate <<- as.Date(NA)  
      components <<- as.character(NA)
      custom_fields <<- list()
      
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
  

  if(!is.null(description)){
    
    if(is.list(description)){
      
      desc_classes <- lapply(description, class)
      df_inds <- sapply(desc_classes, function(cl) "data.frame" %in% cl)
      if(any(df_inds))
        description[df_inds] <- sapply(description[df_inds], df_to_jira_table)

    }   
    
    fields$description <- paste(description, collapse = "\n", sep = "\n")
    
  }
  

  if(!is.na(assignee))
    fields$assignee <- c(name = assignee)
  
  if(!is.na(reporter))
    fields$reporter <- c(name = reporter)
  
  if(!is.na(duedate))
    fields$duedate <- format(duedate, "%Y-%m-%d")
  
  if(!is.na(components))
    fields$components <- lapply(components, function(comp) c(name = comp))
  
  if(length(custom_fields) > 0){
    for(i in 1:length(custom_fields))
      fields[[names(custom_fields)[i]]] <- custom_fields[[i]]

  }
    

  
  return(list("fields" = fields))
  
  
})



# Issue$methods(show = function(){
#   
#   print(project)
#   
# })




#' @name post_issue
#' @title post issue to JIRA
#' @param issue An issue object
#' @param jira_url base url to JIRA. Defaults to 'jira/'
#' @param jira_user username for authentication
#' @param jira_password password for authentication
#' @return POST results
#' @export
post_issue <- function(  issue
                         , jira_url = getOption("jira_url")
                         , jira_user = getOption("jira_user")
                         , jira_password = getOption("jira_password")
                         , verbose = getOption("jira_verbose")
){
  
  ## input checking
  if(!"Issue" %in% class(issue))
    stop("issue must be an Issue Object.")
  
  if(is.null(jira_url)) stop("jira_url is NULL")
  if(is.null(jira_user)) stop("jira_user is NULL")
  if(is.null(jira_password)) stop("jira_user is jira_password")
  
  
  x <- issue$to_issue_list()
  
  print(cat(RJSONIO::toJSON(x)))
  
  res <- POST(url = issue_url(jira_url = jira_url),
              body = RJSONIO::toJSON(x),
              authenticate(user = jira_user, password = jira_password, "basic"),
              add_headers("Content-Type" = "application/json"),
              verbose(data_out = verbose, data_in = verbose, info = verbose)
  )
  
  
  res <- content(res, as = "parsed")
  
  return(res)
  
}




#' @name assign_user
#' @title Assign a user to an issue
#' @param issue An existing issue id or key.
#' @param jira_url base url to JIRA. Defaults to 'jira/'
#' @param jira_user username for authentication
#' @param jira_password password for authentication
#' @param verbose FALSE
#' @return POST results
#' @export
assign_user <- function(issue
                        , user
                        , jira_url = getOption("jira_url")
                        , jira_user = getOption("jira_user")
                        , jira_password = getOption("jira_password")
                        , verbose = getOption("jira_verbose")
){
  
  if(length(issue) != 1)
    stop("issue must have length 1.")
  
  if(length(user) != 1)
    stop("user must have length 1.")
  
  url <- paste0(issue_url(jira_url = jira_url), issue, "/assignee")
  
  res <- PUT(url = url,
             body = RJSONIO::toJSON(c("name" = user)),
             authenticate(user = jira_user, password = jira_password, "basic"),
             add_headers("Content-Type" = "application/json"),
             verbose(data_out = verbose, data_in = verbose, info = verbose)
  )
  
  res <- content(res, as = "parsed")
  
  return(res)
  
}




