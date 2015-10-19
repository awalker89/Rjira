



#' @name get_projects
#' @title Get all projects
#' @param jira_url base JIRA url
#' @param jira_user username for authentication
#' @param jira_password password for authentication
#' @param verbose FALSE
#' @return TODO
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



#' @name get_issues
#' @title Get issues assigned to a user
#' @param user user to search for
#' @param issue An existing issue id or key.
#' @param jira_url base JIRA url
#' @param jira_user username for authentication
#' @param jira_password password for authentication
#' @return character vector of issues
#' @export
#' @seealso \code{\link{search_url}}
#' @examples
#' get_issues(user = "admin")
get_issues <- function(user = NULL
                       , issue
                       , project_key = getOption("jira_project")
                       , jira_url = getOption("jira_url")
                       , jira_user = getOption("jira_user")
                       , jira_password = getOption("jira_password")
                       , verbose = getOption("jira_verbose")){
  
  if(is.null(jira_url))
    stop('jira_url is NULL. See getOption("jira_url")')
  
  if(is.null(jira_user))
    stop("jira_user is NULL")
  
  if(is.null(jira_password))
    stop("jira_password is NULL")
  
  url <- paste0(search_url(jira_url = jira_url), sprintf('jql=project="%s"', project_key))
  if(!is.null(user)){
    url <- paste(url, sprintf('AND assignee="%s"', user))
  }
  
  res <- jira_get(url = url, user = jira_user, password = jira_password, verbose = verbose)
  res <- content(res, as = "parsed")
  res <- res$issues
  

  
  return(res)
  
}


