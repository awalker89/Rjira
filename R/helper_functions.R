


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
