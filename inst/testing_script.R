



if(FALSE){
  
  
  ## Not sure about this
  
  ## How this could work
  # we have an issue object
  # eveytime you change something on the issue object a request is sent to change OR 
  # you do what you want and then send it at the send it at the end
  
  
  ## EXAMPLE SCRIPT
  
  ## setup
  options("jira_user" = "alwalker") ## my username is admin
  options("jira_password" = "alex1")
  options("jira_url" = "jira")
  options("jira_project" = "ADM")
  
  #### -------------------------------------------
  ## Testing variables
  jira_url = getOption("jira_url")
  jira_user = getOption("jira_user")
  jira_password = getOption("jira_password")
  verbose = getOption("jira_verbose")
  project_key = getOption("jira_project")
  #### -------------------------------------------
  
  
  devtools::install_github("awalker89/Rjira")
  

  issue <- create_issue()
  issue$project_key <- "ADM"
  issue$assignee <- "alwalker"
  issue$issue_type <- "Task"
  issue$summary <- "TEST 3"
  issue$description <- list("This look concerning", head(iris, 4))
  issue$components <- "New AVM"
  issue$custom_fields <- list("customfield_11131" = "ADM-283", ## EPIC LINK
                              "customfield_11130" = "115") ## SPRINT ID
  
  post_issue(issue) ## post the issue to JIRA 
  
  
  
  
  
  options("jira_user" = "admin") ## my username is admin
  options("jira_password" = "xxxx")
  options("jira_url" = "jira/")
  options("jira_project" = "BAS")
  
  ## write comment to issue
  add_comment(issue = "BAS-1", comment = "A comment")
  
  my_comment <- "
    Bullet points example
    * point 1
    ** point 1.1
    * point 2
  "
  add_comment(issue = "BAS-1", comment = my_comment) ## formatting works
  
  
  ## write a data.frame as a table
  my_results_comment <- list("The results of this analysis:", head(iris, 4))
  add_comment(issue = "BAS-1", comment = my_results_comment) ## formatting works
  
  
  
  

  
  
  ## Get a list of projects
  get_projects() ## needt to clean up output from here
  
  ## Get list of issues under a project
  issues <- get_issues(project = "ADM", issue = "ADM-323") ## need to parse this to issue objects
  
  ## Get list of issues assigned to user
  res <- get_issues(user = "bwalker" , project_key = getOption("jira_project"))
  sapply(res, "[[", "id")
  sapply(res, "[[", "key")
  
  
  ### WATCHERS
  ## add a watcher to an existing ticket
  add_watcher(issue = "ADM-1", user = "admin")
  get_watchers(issue = "ADM-1")
  remove_watcher(issue = "ADM-1", user = "blance")
  
  get_watchers(issue = "ADM-1")
  get_watchers(issue = "10000")
  
  
  res <- get_comments(issue = "ADM-1")
  res[[1]]
  
  
  
  names(res)
  
  
  ## Assign User
  assign_user(issue = "10000", user = "admin", verbose = TRUE)
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
