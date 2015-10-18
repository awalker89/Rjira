
## Not sure about this


Issue <- setRefClass(

  Class = "Issue",

  fields = list(

      project = "character"
    , issue_type = "character"
    , summary = "character"
    , assignee = "character"


  )

  ,methods = list(

    initialize = function(project = options("jira_project" = "ADM")){

      project <<- project

    }



  )

)
