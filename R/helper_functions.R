





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
  
  search_url <- file.path(jira_url, "rest/api/latest/search?")
  
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
  
  issue_url <- file.path(jira_url, "rest/api/latest/issue/")
  
  return(issue_url)
  
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




