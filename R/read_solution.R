read_solution <- function(cplexSolutionFileName = "results_cplex.txt", variables = variables, logFile = "cplex.log"){
  
  solution <- read.delim(file = cplexSolutionFileName)
  solution[, 1] <- as.character(solution[, 1])
  objective_value <- as.numeric(strsplit(x = solution[7, 1], split = "=", fixed = TRUE)[[1]][2])
  idxVarStart <- which(grepl(pattern = "<variables>", x = solution[, 1]))[-1]
  idxVarEnd <- which(grepl(pattern = "</variables>", x = solution[, 1]))[-1]
  
  solMatrix <- matrix(data = , nrow = idxVarEnd[1]-idxVarStart[1]-1, 
                      ncol = length(idxVarStart))
  colnames(solMatrix) <- paste0("Solution-", seq_len(ncol(solMatrix)))
  ss1 <- sapply(strsplit(solution[seq(from = idxVarStart[1]+1, 
                                      to = idxVarEnd[1]-1, by = 1), 1], 
                         split = " "), "[", 5)
  rownames(solMatrix) <- sapply((strsplit(ss1, split = "=")), "[", 2)
  
  for(ii in seq_len(ncol(solMatrix))){
    
    ss1 <- 
      sapply(strsplit(solution[seq(from = idxVarStart[ii]+1, 
                                   to = idxVarEnd[ii]-1, by = 1), 1], 
                      split = " "), "[", 7)
    solMatrix[, ii] <- 
      gsub(pattern = "/>", replacement = "", 
           x = sapply(strsplit(ss1, split = "="), "[", 2))
    
  }
  
  assignedVar <- solMatrix[which(rownames(solMatrix)%in%variables$var[which(grepl(pattern = "Assigned", x = variables$var_exp))]), 1]
  nn <- c()
  vv <- c()
  for(ii in 1:length(assignedVar)){
    
    exp <- variables$var_exp[which(variables$var==names(assignedVar)[ii])]
    ss <- strsplit(x = exp, split = " ", fixed = TRUE)[[1]]
    nn <- c(nn, paste0(gsub(pattern = "_", replacement = "", x = ss[3], fixed = TRUE), "::", ss[6]))
    vv <- c(vv, as.numeric(assignedVar[ii]))
    
  }
  
  names(vv) <- nn
  
  #
  log <- read.delim(file = logFile)
  idx <- which(grepl(pattern = "Solution time", x = log[, 1]))
  time <- as.character(log[idx, 1])
  time <- strsplit(x = time, split = " = ", fixed = TRUE)[[1]][2]
  time <- as.numeric(strsplit(x = time, split = " sec.", fixed = TRUE)[[1]][1])
  
  #
  returnList <- list()
  returnList[[length(returnList)+1]] <- objective_value
  returnList[[length(returnList)+1]] <- vv
  returnList[[length(returnList)+1]] <- time
  names(returnList) <- c("objective value", "solution", "time")
  return(returnList)
}