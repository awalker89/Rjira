


##


## create an issue

if(FALSE){
  
  
  ## EXAMPLE SCRIPT
  
  ## setup
  options("jira_user" = "admin") ## my username is admin
  options("jira_password" = "alex1")
  options("jira_url" = "https://rwrapper.atlassian.net")
  options("jira_project" = "ADM")
  
  ## Testing variables
  jira_url = getOption("jira_url")
  jira_user = getOption("jira_user")
  jira_password = getOption("jira_password")
  verbose = getOption("jira_verbose")
  project_key = getOption("jira_project")
  
  
  ## create a new issue
  issue <- Issue$new()
  issue$project_key <- "ADM"
  issue$assignee <- "admin"
  issue$issue_type <- "Bug"
  issue$summary = "Testing using reference class objects"
  issue$description = "Test description for reference class Issue"
  
  ## Actually create the ticket on JIRA
  post_issue(issue)
  
  ## Get a list of projects
  get_projects() ## clean up output from here
  
  ## Get list of issues under a project
  get_issues()
  
  ## Get list of issues assigned to user
  
  
  
  
  
  
  
  
  
  
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
  
  
  x <- issue$to_project_list()
  
  POST(url = issue_url(jira_url = jira_url),
       body = RJSONIO::toJSON(x),
       authenticate(user = jira_user, password = jira_password, "basic"),
       add_headers("Content-Type" = "application/json"),
       verbose(data_out = verbose, data_in = verbose, info = verbose)
  )
  
}







jira_get <- function(url = url, user = user, password = password, verbose = verbose){
  
  res <- GET(url = url,
             authenticate(user = user, password = password, "basic"),
             add_headers("Content-Type" = "application/json"),
             verbose(data_out = verbose, data_in = verbose, info = verbose)
  )
  
  return(res)
}





jira_post <- function(x, url, user, password, verbose){
  
  POST(url = url,
       body = RJSONIO::toJSON(x),
       authenticate(user = user, password = password, "basic"),
       add_headers("Content-Type" = "application/json"),
       verbose(data_out = verbose, data_in = verbose, info = verbose)
  )
  
}











