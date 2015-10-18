


##


## create an issue

if(FALSE){


  ## setup
  options("jira_user" = "admin") ## my username is admin
  options("jira_password" = "alex1")
  options("jira_url" = "https://rwrapper.atlassian.net")
  options("jira_project" = "ADM")

  getOption("jira_url")
  getOption("jira_user")
  getOption("jira_password")
  getOption("jira_verbose")


  my_issue <- Issue$new()



  create_issue <- function( project = getOption("jira_project_key")
                           , summary ){

#
#     {
#       "fields": {
#         "project":
#         {
#           "id": "10000"
#         },
#         "summary": "No REST for the Wicked.",
#         "description": "Creating of an issue using ids for projects and issue types using the REST API",
#         "issuetype": {
#           "id": "3"
#         }
#       }
#     }


  }



    x <- list(
      fields = list(
        project = c(key = "ADM"),
        summary = "The quick brown fox jumped over the lazy dog",
        description = "silly old billy",
        issuetype = c(name = "Task")
      )
    )



  POST(url = getOption("jira_url"),
       body = RJSONIO::toJSON(x),
       authenticate(user = getOption("jira_user"), password = getOption("jira_password"), "basic"),
       add_headers("Content-Type" = "application/json"),
       verbose(data_out = TRUE, data_in = TRUE, info = TRUE, ssl = TRUE)
  )







  POST("https://rwrapper.atlassian.net/rest/api/2/issue/",
       body = RJSONIO::toJSON(x),
       authenticate("admin", "alex1", "basic"),
       add_headers("Content-Type" = "application/json"),
       verbose()
  )




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

  search_url <- paste(jira_url, "rest/api/2/search?", sep = "/")

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

  issue_url <- paste(jira_url, "rest/api/2/issue/", sep = "/")

  return(issue_url)

}




