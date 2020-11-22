write_of <- function(transport = transport, variables = variables, network = network){
  
  coef <- c()
  
  for(ii in 1:length(transport)){
    coef <- c(coef, transport[[ii]]$cost*network$frequency)
  }
  
  objectiveFunctionVec <- paste0(" + ", round(x = coef, digits = 2), " ",
                                 variables$var[which(grepl(pattern = "Assigned", x = variables$var_exp, fixed = TRUE))])
  
  objectiveFunction <- paste(objectiveFunctionVec, collapse = "")
  
  objectiveFunction <- substring(text = objectiveFunction[1], first = 4, 
                                 last = nchar(objectiveFunction))
  
  return(objectiveFunction)
  
}