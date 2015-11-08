





#' @name add_watcher
#' @title Add a watched to an existing issue
#' @param jira_url base url to JIRA. Defaults to 'jira/'
#' @param jira_user username for authentication
#' @param jira_password password for authentication
#' @param verbose FALSE
#' @return POST results
#' @seealso \code{\link{remove_watcher}}
#' @seealso \code{\link{get_watchers}}
#' @export
#' @examples
#'
#' \dontrun{
#'  add_watcher(issue = "BAS-1", user = "admin")
#' }
#' 
add_watcher <- function(issue
                        , user
                        , jira_url = getOption("jira_url")
                        , jira_user = getOption("jira_user")
                        , jira_password = getOption("jira_password")
                        , verbose = getOption("jira_verbose")
){
  
  
  if(length(user) != 1)
    stop("user must have length 1.")
  
  url <- paste0(issue_url(jira_url = jira_url), issue, "/watchers")
  
  res <- POST(url = url,
              body = shQuote(user),
              authenticate(user = jira_user, password = jira_password, "basic"),
              add_headers("Content-Type" = "application/json"),
              verbose(data_out = verbose, data_in = verbose, info = verbose)
  )
  
  res <- content(res, as = "parsed")
  
  
  # Responses - probalby get R to look at report the description
  # STATUS 204: Returned if the watcher was added successfully.
  # STATUS 400: Returned if there is a problem with the received user representation.
  # STATUS 401: Returned if the calling user does not have permission to add the watcher to the issue's list of watchers.
  # STATUS 404: Returned if either the issue or the user does not exist.
  
  return(res)
  
}





#' @name remove_watcher
#' @title Remove a watched to an existing issue
#' @param jira_url base url to JIRA. Defaults to 'jira/'
#' @param jira_user username for authentication
#' @param jira_password password for authentication
#' @param verbose FALSE
#' @return DELETE results
#' @seealso \code{\link{add_watcher}}
#' @seealso \code{\link{get_watchers}}
#' @export
#' @examples 
#'
#' \dontrun{
#'    remove_watcher(issue = "BAS-1", user = "admin")
#' }
#' 
remove_watcher <- function(issue
                           , user
                           , jira_url = getOption("jira_url")
                           , jira_user = getOption("jira_user")
                           , jira_password = getOption("jira_password")
                           , verbose = getOption("jira_verbose")
){
  
  
  if(length(user) != 1)
    stop("user must have length 1.")
  
  if(length(issue) != 1)
    stop("issue must have length 1.")
  
  url <- paste0(issue_url(jira_url = jira_url), issue, "/watchers")
  
  res <- DELETE(url = url,
                body = shQuote(user),
                authenticate(user = jira_user, password = jira_password, "basic"),
                add_headers("Content-Type" = "application/json"),
                verbose(data_out = verbose, data_in = verbose, info = verbose)
  )
  
  res <- content(res, as = "parsed")
  
}



#' @name get_watchers
#' @title Get a list of watchers on an existing issue
#' @param jira_url base url to JIRA. Defaults to 'jira/'
#' @param jira_user username for authentication
#' @param jira_password password for authentication
#' @param verbose FALSE
#' @return DELETE results
#' @seealso \code{\link{add_watcher}}
#' @seealso \code{\link{remove_watcher}}
#' @export
#' @examples 
#'
#' \dontrun{
#'    get_watchers(issue = "BAS-1")
#' }
#' 
get_watchers <- function(issue
                         , jira_url = getOption("jira_url")
                         , jira_user = getOption("jira_user")
                         , jira_password = getOption("jira_password")
                         , verbose = getOption("jira_verbose")
){
  
  
  if(length(issue) != 1)
    stop("issue must have length 1.")
  
  url <- paste0(issue_url(jira_url = jira_url), issue, "/watchers")
  
  res <- GET(url = url,
             authenticate(user = jira_user, password = jira_password, "basic"),
             add_headers("Content-Type" = "application/json"),
             verbose(data_out = verbose, data_in = verbose, info = verbose)
  )
  
  
  res <- content(res, as = "parsed")
  
  return(res$watchers)
  
}



