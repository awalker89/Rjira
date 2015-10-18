


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
  jira_verbose = getOption("jira_verbose")
  
  
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






#' @name get_projects
#' @title Get all projects
#' @param jira_url base JIRA url
#' @param jira_user username for authentication
#' @param jira_password password for authentication
#' @param verbose FALSE
#' @return character vector of issues
#' @export
#' @seealso \code{\link{search_url}}
#' @examples
#' get_projects()
get_projects <- function(jira_url = getOption("jira_url")
                         , jira_user = getOption("jira_user")
                         , jira_password = getOption("jira_password")
                         , verbose = getOption("jira_verbose")){
  
  if(is.null(jira_url))
    stop("jira_url is NULL")
  
  project_url <- paste(jira_url, "rest/api/2/project", sep = "/")
  res <- jira_get(url = project_url, user = jira_user, password = jira_password, verbose = verbose)
  
  res <- content(res, as = "parsed")
  
  return(res)
  
  
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


#' @name get_user_issues
#' @title Get issues assigned to a user
#' @param user user to search for
#' @param jira_url base JIRA url
#' @param jira_user username for authentication
#' @param jira_password password for authentication
#' @return character vector of issues
#' @export
#' @seealso \code{\link{search_url}}
#' @examples
#' get_user_issues(user = "billy")
get_user_issues <- function(user
                            , jira_url = getOption("jira_url")
                            , jira_user = getOption("jira_user")
                            , jira_password = getOption("jira_password")){
  
  if(is.null(jira_url))
    stop('jira_url is NULL. See getOption("jira_url")')
  
  
  if(is.null(jira_user))
    stop("jira_user is NULL")
  
  if(is.null(jira_password))
    stop("jira_password is NULL")
  
  
  x <- list(
    fields = list(
      project = c(key = "ADM"),
      summary = "2 The quick brown fox jumped over the lazy dog",
      description = "silly old billy",
      issuetype = c(name = "Task")
    )
  )
  
  
  url <- paste0(search_url(jira_url = jira_url), "jql=assignee=", user)
  
  jira_post(x = x, url = url, password = password, verbose = verbose)
  
}





#' @name search_url
#' @title Get jira search url
#' @param jira_url base url to jira. Defaults to 'jira/'
#' @return string
#' @export
#' @examples
#' search_url()
search_url <- function(jira_url = getOption("jira_url")){
  
  if(is.null(jira_url))
    return(NULL)
  
  search_url <- paste(jira_url, "rest/api/latest/search?", sep = "/")
  
  return(search_url)
  
}





#' @name issue_url
#' @title Get jira issue url
#' @param jira_url base url to jira. Defaults to 'jira/'
#' @return string
#' @export
#' @examples
#' issue_url()
issue_url <- function(jira_url = getOption("jira_url")){
  
  if(is.null(jira_url))
    return(NULL)
  
  issue_url <- paste(jira_url, "rest/api/latest/issue/", sep = "/")
  
  return(issue_url)
  
}




