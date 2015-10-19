


if(FALSE){
  
  
  ## Not sure about this
  
  ## How this could work
  # we have an issue object
  # eveytime you change something on the issue object a request is sent to change OR 
  # you do what you want and then send it at the send it at the end
  
  
  
  
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
  issue$issue_type <- "Task"
  issue$summary <- "Test Assign to admin"
  issue$description <- "An optional description"
  post_issue(issue) ## post the issue to JIRA 
  
  
  ## write comment to issue
  add_comment(issue = "ADM-6", comment = "A comment", verbose = )
  
  
  my_comment <- "
  I need to do this
  * get_comments()
  ** delete_comment()
  * add_comment()
  "
  add_comment(issue = "ADM-1", comment = my_comment, verbose = TRUE) ## formatting works
  
  my_results_comment <- list("The results of this analysis:", head(iris, 4))
  add_comment(issue = "ADM-1", comment = my_results_comment, verbose = TRUE) ## formatting works
  
  
  
  
  
  
  ## Get a list of projects
  get_projects() ## needt to clean up output from here
  
  ## Get list of issues under a project
  issues <- get_issues(project = "ADM") ## need to parse this to issue objects
  
  ## Get list of issues assigned to user
  res <- get_issues(user = "bwalker" , project_key = getOption("jira_project"))
  sapply(res, "[[", "id")
  sapply(res, "[[", "key")
  
  
  ### WATCHERS
  ## add a watcher to an existing ticket
  add_watcher(issue = "ADM-1", user = "blance")
  get_watchers(issue = "ADM-1", user = "blance")
  remove_watcher(issue = "ADM-1", user = "blance")
  
  get_watchers(issue = "ADM-1")
  get_watchers(issue = "10000")
  
  
  
  ## Assign User
  assign_user(issue = "10000", user = "blance", verbose = TRUE)
  get_assignee(issue = "ADM-1")
  
  
  ## TODO
  # clone issue **** these is a button to do this on the UI.
  # get_assignee()
  # assign issue to a sprint - create ticket within a sprint / move ticket to sprint
  # remove_comment()
  # get_comments()
  
  
  # get issue comments -- GET /rest/api/2/issue/{issueIdOrKey}/comment
  # JQL query function to find issues
 
  # maybe build a dashboard over it to see stats
  
  
  
  
  
  
  ## DONE
  # add_watcher()
  # remove_watcher()
  # get_watchers()
  # comment formatting and add tables - data.frame to comment - writing data.frames to JIRA markup tables would be super handy
  # assign existing issue to someone else: assign_issue()
  
  
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














