

.onLoad <- function(libname, pkgname){
  
  ## Set default options
  
  opts <- c( "jira_url" = '"/jira"'
             ,"jira_verbose" = "FALSE"
             
  )
  
  for (i in setdiff(names(opts),names(options()))){  ## makes sure we don't overwrite existing options
    # print(paste("options(",i,"=",opts[i],")",sep=""))
    eval(parse(text=paste("options(",i,"=",opts[i],")",sep="")))
  }
  
  
}


