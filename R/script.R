


if(FALSE){
  
  
  ## EXAMPLE SCRIPT
  
  ## setup
  options("jira_user" = "admin") ## my username is admin
  options("jira_password" = "alex1")
  options("jira_url" = "https://rwrapper.atlassian.net")
  options("jira_project" = "ADM")
  
  #### -------------------------------------------
  ## Testing variables
  jira_url = getOption("jira_url")
  jira_user = getOption("jira_user")
  jira_password = getOption("jira_password")
  verbose = getOption("jira_verbose")
  project_key = getOption("jira_project")
  #### -------------------------------------------
  
  
  ## create a new issue object in R
  issue <- create_issue()
  issue$project_key <- "ADM"
  issue$assignee <- "admin"
  issue$issue_type <- "Bug"
  issue$summary <- "Testing using reference class objects"
  issue$description <- "Test description for reference class Issue"
  
  post_issue(issue) ## post the issue to JIRA 
  
  ## write comment to issue
  issue_id <- "ADM-1"
  comment <- "this is a test comment via API 2"
  add_c
  
  
  ## Get a list of projects
  get_projects() ## needt to clean up output from here
  
  
  ## Get list of issues under a project
  issues <- get_issues(project = "ADM") ## need to parse this to issue objects
  
  
  
  ## Get list of issues assigned to user
  get_issues(user = "bwalker" , project_key = getOption("jira_project"))
  
  
  ## 
  
  
}


#' @name write_comment
#' @title Comment on an existing issue
#' @param jira_url base url to jira. Defaults to 'jira/'
#' @return string
#' @export
write_comment <- function(issue_id
                          , comment
                          , jira_url = getOption("jira_url")
                          , jira_user = getOption("jira_user")
                          , jira_password = getOption("jira_password")
                          , verbose = getOption("jira_verbose")
){
  
  
  url <- paste0(issue_url(jira_url = jira_url), issue_id, "/comment")
  
  x <- list(body = comment)

  res <- POST(url = url,
       body = RJSONIO::toJSON(x),
       authenticate(user = jira_user, password = jira_password, "basic"),
       add_headers("Content-Type" = "application/json"),
       verbose(data_out = verbose, data_in = verbose, info = verbose)
  )
  
  res <- content(res, as = "parsed")
  
  return(res)
  
}






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
  
  POST(url = issue_url(jira_url = jira_url),
       body = RJSONIO::toJSON(x),
       authenticate(user = jira_user, password = jira_password, "basic"),
       add_headers("Content-Type" = "application/json"),
       verbose(data_out = verbose, data_in = verbose, info = verbose)
  )
  
}














