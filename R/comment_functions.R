



#' @name df_to_jira_table
#' @title Convert data.frame to JIRA markup table
#' @param x a data.frame
#' @return string
#' @export
#' @examples
#'cat( df_to_jira_table(head(iris)))
df_to_jira_table <- function(x){
  
  x <- as.data.frame(x)
  nms <- paste0("||", paste(names(x), collapse = "||"), "||")
  body <- unname(apply(x, MARGIN = 1, FUN = function(x) paste0("|", paste(x, collapse = "|"), "|")))
  body <- paste(nms, paste(body, collapse = "\n"), sep = "\n")
  
  return(body)
  
}




#' @name add_comment
#' @title Comment on an existing issue
#' @param issue An existing issue id or key.
#' @param comment Charactervector, list or data.frame. See examples.
#' @param jira_url base url to JIRA. Defaults to 'jira/'
#' @param jira_user username for authentication
#' @param jira_password password for authentication
#' @param verbose FALSE
#' @return POST results
#' @export
add_comment <- function(issue
                        , comment
                        , jira_url = getOption("jira_url")
                        , jira_user = getOption("jira_user")
                        , jira_password = getOption("jira_password")
                        , verbose = getOption("jira_verbose")
){
  
  if(length(comment) == 0)
    stop("comment must have length > 0")
  
  if(length(comment) > 1){
    comment_classes <- lapply(comment, class)
    
    df_inds <- sapply(comment_classes, function(cl) "data.frame" %in% cl)
    
    if(any(df_inds))
      comment[df_inds] <- sapply(comment[df_inds], df_to_jira_table)
    
    comment <- paste(comment, collapse = "\n", sep = "\n")
    
  }
  
  if(length(issue) != 1)
    stop("issue must have length 1.")
  
  url <- paste0(issue_url(jira_url = jira_url), issue, "/comment")
  
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

